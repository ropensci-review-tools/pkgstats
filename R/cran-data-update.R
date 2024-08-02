#' Update local `data.frame` of `pkgstats` data with data from all packages
#' uploaded to CRAN since creation of local data.
#'
#' Note that this function only updates data from current CRAN packages, and
#' not from archived package. Any packages which have been updated multiple
#' times since generation of previous results will only be updated with the
#' most recent data, and not with data from any intermediate updates.
#'
#' @inheritParams pkgstats_from_archive
#' @param prev_results Result of previous call either to this function, or to
#' \link{pkgstats_from_archive}, as a `data.frame`  of statistics, with one row
#' for each package.
#'
#' @family archive
#' @export
#' @examples
#' # Create fake archive directory with single tarball:
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' tarball <- basename (f)
#'
#' archive_path <- file.path (tempdir (), "archive")
#' if (!dir.exists (archive_path)) {
#'     dir.create (archive_path)
#' }
#' path <- file.path (archive_path, tarball)
#' file.copy (f, path)
#' tarball_path <- file.path (archive_path, "tarballs")
#' dir.create (tarball_path, recursive = TRUE)
#' file.copy (path, file.path (tarball_path, tarball))
#' \dontrun{
#' out <- pkgstats_from_archive (tarball_path)
#' }
pkgstats_update <- function (prev_results = NULL,
                             results_file = NULL,
                             chunk_size = 1000L,
                             num_cores = 1L,
                             save_full = FALSE,
                             save_ex_calls = FALSE,
                             results_path = tempdir ()) {

    requireNamespace ("callr")
    requireNamespace ("hms")
    requireNamespace ("httr2")
    requireNamespace ("parallel")

    checkmate::assert_int (chunk_size, lower = 1L)
    checkmate::assert_int (num_cores, lower = 1L)
    checkmate::assert_logical (save_full)
    checkmate::assert_scalar (save_full)
    checkmate::assert_logical (save_ex_calls)
    checkmate::assert_scalar (save_ex_calls)
    checkmate::assert_string (results_path)

    if (!is.null (prev_results)) {
        if (!identical (names (prev_results), names (pkgstats_summary ()))) {
            stop (
                "'prev_results' must contain a ",
                "'data.frame' of 'pkgstats' summaries"
            )
        }
    }

    res <- results_files <- NULL
    out <- prev_results

    new_cran_pkgs <- list_new_cran_updates (prev_results)

    npkgs <- length (new_cran_pkgs)

    if (npkgs > 0) {

        n <- ceiling (npkgs / chunk_size)
        n <- factor (rep (seq (n), each = chunk_size)) [seq (npkgs)]
        new_cran_pkgs <- split (new_cran_pkgs, f = n)

        message (
            "Starting trawl of ", npkgs,
            " files in ", length (new_cran_pkgs), " chunks"
        )

        results_path <- normalizePath (results_path, mustWork = FALSE)
        if (!dir.exists (results_path)) {
            dir.create (results_path, recursive = TRUE)
        }

        index <- 1 # name of temporary files
        pt0 <- proc.time ()

        for (f in new_cran_pkgs) {

            if (num_cores > 1L) {

                res <- parallel::mclapply (f, function (i) {

                    one_summary_from_cran (
                        i,
                        save_full,
                        save_ex_calls,
                        results_path
                    )

                }, mc.cores = num_cores)

            } else {

                res <- lapply (f, function (i) {

                    one_summary_from_cran (
                        i,
                        save_full,
                        save_ex_calls,
                        results_path
                    )
                })
            }

            fname <- file.path (
                results_path,
                paste0 ("pkgstats-results-", index, ".Rds")
            )
            saveRDS (do.call (rbind, res), fname)
            results_files <- c (results_files, fname)

            archive_trawl_progress_message (index, chunk_size, npkgs, pt0)
            index <- index + 1
        }

        res <- do.call (rbind, lapply (results_files, readRDS))
    }

    if (inherits (out$date, "POSIXt")) {
        res$date <- as.POSIXct (res$date, "%y-%m-%d %H-%M-%S")
    }

    out <- rbind (out, res)
    out <- out [which (!is.na (out$package)), ]
    rownames (out) <- NULL

    if (!is.null (results_files)) {
        chk <- file.remove (results_files) # nolint
    }

    if (!is.null (res) && !is.null (results_file)) {

        results_file <- archive_results_file_name (results_file)
        saveRDS (out, results_file)
    }

    invisible (out)
}

list_new_cran_updates <- function (prev_results) {

    prev_tarball <- paste0 (prev_results$package, "_", prev_results$version)
    cran_pkgs <- tools::CRAN_package_db ()
    cran_tarball <- paste0 (cran_pkgs$Package, "_", cran_pkgs$Version)

    cran_pkgs <- cran_pkgs [which (!cran_tarball %in% prev_tarball), ]
    paste0 (cran_pkgs$Package, "_", cran_pkgs$Version)

}

one_summary_from_cran <- function (i,
                                   save_full = FALSE,
                                   save_ex_calls = FALSE,
                                   results_path = tempdir ()) {

    cran_url <- "https://cran.r-project.org/src/contrib/"
    tarball <- paste0 (i, ".tar.gz")
    url <- paste0 (cran_url, tarball)
    path <- fs::path (fs::path_temp (), tarball)

    req <- httr2::request (url) |>
        httr2::req_headers ("Accept" = "application/octet-stream")
    resp <- httr2::req_perform (req)

    if (httr2::resp_is_error (resp)) {
        return (NULL)
    }

    writeBin (httr2::resp_body_raw (resp), path)

    s <- one_summary_from_archive (
        path = path,
        save_full = save_full,
        save_ex_calls = save_ex_calls,
        results_path = results_path
    )

    unlink (path)

    return (s)
}

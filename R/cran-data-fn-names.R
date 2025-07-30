#' Trawl a local CRAN archive to extract function names only from all packages
#'
#' @inheritParams pkgstats_from_archive
#' @return A `data.frame` object with one row for each function in each package
#' and the following columns:
#' \itemize{
#' \item Package name
#' \item Package version
#' \item Function name
#' }
#'
#' @family archive
#' @export
pkgstats_fns_from_archive <- function (path,
                                       archive = FALSE,
                                       prev_results = NULL,
                                       results_file = NULL,
                                       chunk_size = 1000L,
                                       num_cores = 1L,
                                       results_path = fs::path_temp ()) {

    requireNamespace ("hms", quietly = TRUE)
    requireNamespace ("parallel", quietly = TRUE)

    if (!grepl ("tarball", path)) {
        if (!fs::dir_exists (fs::path (path, "tarballs"))) {
            stop ("path must contain a 'tarballs' directory")
        }
        path <- fs::path (path, "tarballs")
    }

    if (basename (path) != "tarballs") {
        stop ("path must be a directory named 'tarballs'")
    }

    if (!fs::dir_exists (path)) {
        stop ("[", path, "] directory does not exist")
    }

    res <- NULL
    out <- prev_results

    flist <- fs::dir_ls (
        path,
        recurse = archive,
        regexpr = "\\.tar\\.gz$"
    )
    flist <- expand_path (flist)
    flist <- rm_prev_files (flist, prev_results)
    nfiles <- length (flist)

    if (nfiles > 0) {

        n <- ceiling (nfiles / chunk_size)
        n <- factor (rep (seq (n), each = chunk_size)) [seq (nfiles)]
        flist <- split (flist, f = n)

        message (
            "Starting trawl of ", nfiles,
            " files in ", length (flist), " chunks"
        )

        results_path <- expand_path (results_path)
        if (!fs::dir_exists (results_path)) {
            fs::dir_create (results_path)
        }
        results_files <- NULL

        index <- 1 # name of temporary files
        pt0 <- proc.time ()

        for (f in flist) {

            res <- parallel::mclapply (f, function (i) {

                tryCatch (
                    pkgstats::pkgstats_fn_names (i),
                    error = function (e) NULL
                )

            }, mc.cores = num_cores)

            fname <- fs::path (
                results_path,
                paste0 ("pkgstats-fn-names-results-", index, ".Rds")
            )
            saveRDS (do.call (rbind, res), fname)
            results_files <- c (results_files, fname)

            prog <- index * chunk_size / nfiles
            prog_fmt <- format (100 * prog, digits = 2)
            pt1 <- as.integer ((proc.time () - pt0) [3])
            t_per_file <- pt1 / (index * chunk_size)
            t_total <- t_per_file * nfiles
            t_rem <- hms::hms (t_total - pt1)

            ndone <- min (c (nfiles, index * chunk_size))

            message (
                "[", ndone, " / ", nfiles,
                "]  = ", prog_fmt, "%; (elapsed, remaining) = (",
                pt1, ", ", t_rem, ")"
            )

            index <- index + 1
        }

        res <- do.call (rbind, lapply (results_files, readRDS))
    }

    t_total <- hms::hms (as.integer (proc.time () [3] - pt0 [3]))
    message ("\nTotal time = ", t_total)

    out <- rbind (out, res)
    rownames (out) <- NULL

    chk <- fs::file_delete (results_files) # nolint

    if (!is.null (res) && !is.null (results_file)) {

        if (!grepl (.Platform$file.sep, results_file)) {
            results_file <- fs::path (".", results_file)
        }
        results_file <- expand_path (results_file)

        results_path <- gsub (
            basename (results_file), "",
            results_file
        )
        results_path <- expand_path (results_path)
        if (!fs::dir_exists (results_path)) {
            stop ("Directory [", results_path, "] does not exist")
        }

        results_file <- basename (results_file)
        results_file <- tools::file_path_sans_ext (results_file)
        results_file <- fs::path (
            results_path,
            paste0 (results_file, ".Rds")
        )

        saveRDS (out, results_file)
    }

    invisible (out)
}

#' Update function names data from previous data and newly updated CRAN
#' packages only.
#'
#' @inheritParams pkgstats_from_archive
#' @return A `data.frame` object with one row for each function in each package
#' and the following columns:
#' \itemize{
#' \item Package name
#' \item Package version
#' \item Function name
#' }
#'
#' @family archive
#' @export

# nocov start
pkgstats_fns_update <- function (prev_results = NULL,
                                 results_file = NULL,
                                 chunk_size = 1000L,
                                 num_cores = 1L,
                                 results_path = tempdir ()) {

    requireNamespace ("hms", quietly = TRUE)
    requireNamespace ("parallel", quietly = TRUE)

    checkmate::assert_int (chunk_size, lower = 1L)
    checkmate::assert_int (num_cores, lower = 1L)
    checkmate::assert_string (results_path)
    check_prev_results (prev_results)

    res <- NULL
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

        results_path <- expand_path (results_path)
        if (!fs::dir_exists (results_path)) {
            fs::dir_create (results_path, recurse = TRUE)
        }

        index <- 1 # name of temporary files
        pt0 <- proc.time ()

        for (f in new_cran_pkgs) {

            if (num_cores > 1L) {

                res <- parallel::mclapply (f, function (i) {

                    tryCatch (
                        pkgstats::pkgstats_fn_names (i),
                        error = function (e) NULL
                    )

                }, mc.cores = num_cores)

            } else {

                res <- lapply (f, function (i) {

                    tryCatch (
                        pkgstats::pkgstats_fn_names (i),
                        error = function (e) NULL
                    )
                })
            }

            fname <- fs::path (
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

    out <- rbind (out, res)

    tarball <- paste0 (out$package, "_", out$version)
    cran_pkgs <- get_cran_db ()
    cran_tarball <- paste0 (cran_pkgs$Package, "_", cran_pkgs$Version)
    index <- which (tarball %in% cran_tarball)
    out <- out [index, ]

    if (!is.null (res) && !is.null (results_file)) {

        results_file <- archive_results_file_name (results_file)
        saveRDS (out, results_file)
    }

    invisible (out)
}
# nocov end

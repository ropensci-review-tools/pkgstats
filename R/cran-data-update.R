RELEASE_TAG <- "v0.1.6"

#' Update pkgstats` data on GitHub release
#'
#' This function is intended for internal rOpenSci use only. Usage by any
#' unauthorized users will error and have no effect unless run with `upload =
#' FALSE`, in which case updated data will be created in the sub-directory
#' "pkgstats-results" of R's current temporary directory.
#'
#' @param upload If `TRUE`, upload updated results to GitHub release.
#' @return Local path to directory containing updated results.
#' @family archive
#' @export

# nocov start
pkgstats_update <- function (upload = TRUE) {

    requireNamespace ("callr", quietly = TRUE)
    requireNamespace ("hms", quietly = TRUE)
    requireNamespace ("httr2", quietly = TRUE)
    requireNamespace ("piggyback", quietly = TRUE)

    results_path <- fs::dir_create (fs::path (fs::path_temp (), "pkgstats-results"))

    stats_prev_path <- dl_prev_data (results_path, what = "all")
    stats_prev <- readRDS (stats_prev_path)
    fn_names_prev_path <- dl_prev_data (results_path, what = "fn_names")
    fn_names_prev <- readRDS (fn_names_prev_path)

    check_prev_results (stats_prev)
    check_prev_results (fn_names_prev)

    new_cran_pkgs <- list_new_cran_updates (stats_prev)

    npkgs <- length (new_cran_pkgs)

    if (npkgs == 0) {
        return (NULL)
    }

    message (
        "Downloading and analysing ", npkgs, " packages."
    )

    pt0 <- proc.time ()

    res <- lapply (seq_along (new_cran_pkgs), function (p) {

        stats <- fn_names <- NULL

        tarball_path <- dl_one_tarball (results_path, new_cran_pkgs [p])
        if (!is.null (tarball_path) && fs::file_exists (tarball_path)) {

            stats <- one_summary_from_archive (
                tarball_path,
                save_full = FALSE,
                save_ex_calls = FALSE,
                results_path
            )
            fn_names <- tryCatch (
                pkgstats::pkgstats_fn_names (tarball_path),
                error = function (e) NULL
            )

            tarball_dir <- gsub ("\\.tar\\.gz$", "", tarball_path)
            unlink (tarball_dir, recursive = TRUE)
            unlink (tarball_path, recursive = TRUE)
        }

        archive_trawl_progress_message (p, 1, npkgs, pt0)

        list (stats = stats, fn_names = fn_names)
    })

    stats <- do.call (rbind, lapply (res, function (i) i$stats))
    fn_names <- do.call (rbind, lapply (res, function (i) i$fn_names))

    if (!inherits (stats$date, "POSIXt")) {
        stats$date <- as.POSIXct (stats$date, "%y-%m-%d %H-%M-%S")
    }

    stats <- rbind (stats_prev, stats [which (!is.na (stats$package)), ])
    stats_current <- pkgstats_cran_current_from_full (stats)
    fn_names <- rbind (fn_names_prev, fn_names [which (!is.na (fn_names$package)), ])

    # Reduce fn_names to only current pkgs:
    stats_pkgs_current <- paste0 (stats_current$package, "_", stats_current$version)
    fn_nm_pkgs <- paste0 (fn_names$package, "_", fn_names$version)
    fn_names <- fn_names [which (fn_nm_pkgs %in% stats_pkgs_current), ]

    dat <- list (
        "pkgstats-CRAN-all.Rds" = stats,
        "pkgstats-CRAN-current.Rds" = stats_current,
        "pkgstats-fn-names.Rds" = fn_names
    )

    for (i in seq_along (dat)) {
        fpath <- fs::path (results_path, names (dat) [i])
        saveRDS (dat [[i]], fpath)
        piggyback::pb_upload (
            file = fpath,
            repo = "ropensci-review-tools/pkgstats",
            tag = RELEASE_TAG
        )
    }
}
# nocov end

dl_prev_data <- function (results_path, what = "all") {
    what <- match.arg (what, c ("all", "current", "fn_names"))
    files <- c (
        all = "pkgstats-CRAN-all.Rds",
        current = "pkgstats-CRAN-current.Rds",
        fn_names = "pkgstats-fn-names.Rds"
    )
    f <- files [what]
    path <- piggyback::pb_download (
        file = f,
        repo = "ropensci-review-tools/pkgstats",
        dest = results_path,
        tag = RELEASE_TAG
    )
    if (!is.null (path)) {
        path <- path [[1]]$request$output$path
    } else { # downloaded files already exist
        path <- fs::dir_ls (results_path, regexp = f, fixed = TRUE)
    }
    return (path)
}

get_cran_db <- memoise::memoise (tools::CRAN_package_db)

dl_one_tarball <- function (results_path, tarball) {

    cran_url <- "https://cran.r-project.org/src/contrib/"
    tarball <- paste0 (tarball, ".tar.gz")
    url <- paste0 (cran_url, tarball)
    path <- fs::path (results_path, tarball)

    if (fs::file_exists (path)) {
        return (NULL)
    }

    # No native pipe here...
    req <- httr2::request (url)
    req <- httr2::req_headers (req, "Accept" = "application/octet-stream")
    resp <- httr2::req_perform (req)

    if (httr2::resp_is_error (resp)) {
        return (NULL)
    }

    writeBin (httr2::resp_body_raw (resp), path)
    return (path)
}

list_new_cran_updates <- function (prev_results) {

    prev_tarball <- unique (paste0 (prev_results$package, "_", prev_results$version))
    cran_pkgs <- get_cran_db ()
    cran_tarball <- paste0 (cran_pkgs$Package, "_", cran_pkgs$Version)

    cran_pkgs <- cran_pkgs [which (!cran_tarball %in% prev_tarball), ]
    ret <- character (0L)
    if (nrow (cran_pkgs) > 0) {
        ret <- paste0 (cran_pkgs$Package, "_", cran_pkgs$Version)
    }
    return (ret)
}

check_prev_results <- function (prev_results) {
    msg <- paste0 (
        "'prev_results' must be given, and must be a 'data.frame' from a ",
        "previous call to 'pkgstats_from_archive()' or 'pkgstats_update()'."
    )
    if (is.null (prev_results)) stop (msg, call. = FALSE)
    if (!is.null (prev_results)) {
        if (!inherits (prev_results, "data.frame")) stop (msg, call. = FALSE)
        if (nrow (prev_results) < 20000) stop (msg, call. = FALSE)
        if (ncol (prev_results) > 3L) {
            if (ncol (prev_results) != ncol (null_stats ())) stop (msg, call. = FALSE)
            if (!identical (names (prev_results), names (pkgstats_summary ()))) {
                stop (msg, call. = FALSE)
            }
        } else {
            if (ncol (prev_results) != 3L) stop (msg, call. = FALSE)
            nms <- c ("package", "version", "fn_name")
            if (!identical (names (prev_results), nms)) {
                stop (msg, call. = FALSE)
            }
        }
    }
}

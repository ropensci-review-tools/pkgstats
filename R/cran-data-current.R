#' Reduce `data.frame` of full CRAN archive data to current packages only.
#'
#' @inheritParams pkgstats_from_archive
#' @return A `data.frame` object with one row for each package containing
#' summary statistics generated from the \link{pkgstats_summary} function.
#'
#' @family archive
#' @export
pkgstats_cran_current_from_full <- function (prev_results, results_file = NULL) {

    prev_tarball <- paste0 (prev_results$package, "_", prev_results$version)

    cran_pkgs <- get_cran_db ()
    cran_tarball <- paste0 (cran_pkgs$Package, "_", cran_pkgs$Version)

    index <- which (prev_tarball %in% cran_tarball)
    res <- prev_results [index, ]

    if (!is.null (res) && !is.null (results_file)) {

        results_file <- archive_results_file_name (results_file)
        saveRDS (res, results_file)
    }

    return (res)
}

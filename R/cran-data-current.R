#' Reduce `data.frame` of full CRAN archive data to current packages only.
#'
#' @inheritParams pkgstats_from_archive
#' @family archive
#' @export
pkgstats_cran_current_from_full <- function (prev_results) {

    prev_tarball <- paste0 (prev_results$package, "_", prev_results$version)

    cran_pkgs <- tools::CRAN_package_db ()
    cran_tarball <- paste0 (cran_pkgs$Package, "_", cran_pkgs$Version)

    index <- which (prev_tarball %in% cran_tarball)
    prev_results [index, ]
}

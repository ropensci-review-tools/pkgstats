#' Function to update CRAN data and push to GitHub.
#'
#' This function is intended for internal rOpenSci usage only.
#' @export
pkgstats_cran_data_github <- function () {

    requireNamespace ("piggyback")

    fname <- "pkgstats-CRAN-all.Rds"
    path <- piggyback::pb_download (file = fname, dest = tempdir (), tag = "v0.1.6")
    results_file <- path [[1]]$request$output$path
    prev_results <- readRDS (path)
    cran_archive <- pkgstats_update (prev_results = prev_results, results_file = results_file)
    pb_upload (file = results_file, tag = "v0.1.6")

    results_file <- fs::path (fs::path_temp (), "pkgstats-CRAN-current.Rds")
    cran_current <- pkgstats_cran_current_from_full (cran_archive, results_file = results_file)
    pb_upload (file = results_file, tag = "v0.1.6")

    fname <- "pkgstats-fn-names.Rds"
    path <- piggyback::pb_download (file = fname, dest = tempdir (), tag = "v0.1.6")
    results_file <- path [[1]]$request$output$path
    cran_fns <- pkgstats_fns_from_archive (path)
}

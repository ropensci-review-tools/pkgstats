#' Get lines-of-code ('cloc') statistics for a package
#'
#' @param path Path to locally extracted tarball of package, obtained from
#' \link{extract_tarball}.
#' @return A 'data.frame' with statistics from the four main directories (where
#' present) of '/R', '/src', '/inst/include', and 'vignettes'
#' @export
#' @examples
#' \dontrun{
#' tarball <- "magrittr_2.0.1.tar.gz"
#' u <- paste0 ("https://cran.r-project.org/src/contrib/",
#'              tarball)
#' dest <- file.path (tempdir (), tarball)
#' download.file (u, dest)
#' path <- extract_tarball (dest)
#' cloc_stats (path)
#' }
cloc_stats <- function (path) {

    if (!file.exists (path))
        stop (paste0 ("path [", path, "] does not exist. ",
                      "Did you first 'extract_tarball()'?"))

    paths <- c (file.path (path, "R"),
                file.path (path, "src"),
                file.path (path, "inst", "include"),
                file.path (path, "vignettes"))

    out <- lapply (paths, function (i) {
                       res <- NULL
                       if (dir.exists (i))
                           res <- cloc::cloc (i)
                       return (res) })

    do.call (rbind, out)
}

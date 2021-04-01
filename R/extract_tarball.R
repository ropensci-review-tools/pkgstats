#' Extract tarball of a package into temp directory and return path to extracted
#' package
#'
#' @param tarball Full path to local tarball of an R package
#' @return Path to extracted version of package
#' @export
#' @examples
#' \dontrun{
#' tarball <- "magrittr_2.0.1.tar.gz"
#' u <- paste0 ("https://cran.r-project.org/src/contrib/",
#'              tarball)
#' f <- file.path (tempdir (), tarball)
#' download.file (u, f)
#' path <- extract_tarball (f)
#' }
extract_tarball <- function (tarball) {

    if (!file.exists (tarball))
        stop ("file [", tarball, "] does not exist")
    if (!checkmate::testCharacter (tarball,
                                   len = 1,
                                   pattern = "\\.tar\\.gz$"))
        stop (paste0 ("tarball must be a single character ",
                      "specifying path to .tar.gz file"))

    flist <- utils::untar (tarball,
                           exdir = tempdir (),
                           list = TRUE)
    if (utils::untar (tarball, exdir = tempdir ()) != 0)
        stop ("Unable to extract tarball to 'tempdir'")

    path <- file.path (tempdir (), flist [1])

    return (path)
}

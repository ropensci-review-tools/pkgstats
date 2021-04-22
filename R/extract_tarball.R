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

    path <- normalizePath (file.path (tempdir (), flist [1]))

    chk <- rename_files_in_r (path)
    if (!chk)
        warning ("Files in .R directory unable to be re-named")

    return (path)
}

#' files in './R' directory may have suffixes of .q, .r, or .s 
#' https://cran.r-project.org/doc/manuals/R-exts.html#Package-subdirectories
#' But parsers + cloc only recognise .R/.r, so rename any others
#' Discovered via BDR's
#' https://cran.r-project.org/web/packages/boot/index.html
#' @noRd
rename_files_in_r <- function (path) {

    fr <- normalizePath (list.files (file.path (path, "R"),
                                     full.names = TRUE))

    index <- which (!grepl ("\\.r$", fr))

    if (length (index) > 0) {

        chk <- NULL

        for (f in fr [index]) {

            chk <- c (chk,
                      file.rename (f, gsub ("\\.[a-zA-Z]$", ".R", f)))
        }
    }

    return (all (chk))
}

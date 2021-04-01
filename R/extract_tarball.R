#' Extract tarball of a package into temp directory and return path to extracted
#' package
#'
#' @param tarball Full path to local tarball of an R package
#' @return Path to extracted version of package
#' @export
extract_tarball <- function (tarball) {

    flist <- utils::untar (tarball, exdir = tempdir (), list = TRUE)

    return (file.path (tempdir (), flist [1]))
}

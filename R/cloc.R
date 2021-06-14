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

    check_path (path)

    paths <- c (file.path (path, "R"),
                file.path (path, "src"),
                file.path (path, "inst", "include"),
                file.path (path, "tests"),
                file.path (path, "vignettes"))

    out <- lapply (paths, function (i) {
                       res <- NULL
                       if (dir.exists (i)) {
                           res <- cloc::cloc (i)
                           res <- res [which (res$language != "SUM"), , drop = FALSE]
                       }
                       return (res) })

    do.call (rbind, out)
}

#' Count white space statitics from one specified directory
#' @param path A directory
#' @return A named vector of 5 values
#' @noRd
cloc_stats1 <- function (path) {

    if (dir.exists (path)) {

        flist <- normalizePath (list.files (path, full.names = TRUE))
        flist <- flist [which (!grepl ("\\.o$|\\.so$", flist))]
        s <- cpp_cloc (flist)

        leading_white <- s [5:length (s)]
        indentation <- 1L + which.max (leading_white [-1])
    } else {

        s <- rep (NA_integer_, 4L)
        indentation <- NA_integer_
    }

    c (nlines = s [1],
       nempty = s [2],
       nspaces = s [3],
       nchrars = s [3] + s [4],
       indentation = indentation)
}

#' Statistics on white space in code
#'
#' @param path Directory to package being analysed
#' @return A list of statistics for each of three directories, 'R', 'src', and
#' 'inst/include', each one having 5 statistics of total numbers of lines,
#' numbers of empty lines, total numbers of white spaces, total numbers of
#' characters, and indentation used in files in that directory.
#' 
#' @note NA values are returned for directories which do not exist.
#' @export
whitespace_stats <- function (path) {

    dirs <- c ("R", "src", "inst")
    paths <- file.path (path, dirs)
    paths [3] <- file.path (paths [3], "include")

    dirs <- vapply (strsplit (paths, .Platform$file.sep),
                    function (i) utils::tail (i, 1L),
                    character (1))
    stats <- lapply (dirs, function (i) {
                         res <- cloc_stats1 (file.path (path, i))
                         names (res) <- paste0 (i, "_", names (res))
                         return (res)   })
    stats <- unlist (stats)
    names (stats) <- gsub ("^include", "inst", names (stats))

    return (stats)
}

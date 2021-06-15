
#' Count white space statitics from one specified directory
#' @param path A directory
#' @return A named vector of 5 values
#' @noRd
loc_stats1 <- function (path) {

    if (dir.exists (path)) {

        flist <- normalizePath (list.files (path, full.names = TRUE))
        flist <- flist [which (!grepl ("\\.o$|\\.so$", flist))]
        exts <- vapply (flist, function (i)
                        utils::tail (strsplit (i, "\\.") [[1]], 1),
                        character (1))
        ftypes <- file_exts (exts)
        ftypes$cmt <- gsub ("\\^\\\\s\\*", "", ftypes$cmt)
        s <- cpp_loc (flist,
                      ftypes$cmt_open,
                      ftypes$cmt_close,
                      ftypes$cmt)

        leading_white <- s [8:length (s)] # rm 1st value
        indentation = average_leading_white (leading_white)

    } else {

        s <- rep (NA_integer_, 4L)
        indentation <- NA_integer_
    }

    c (nlines = s [1],
       ncode = s [2],
       ndoc = s [3],
       nempty = s [4],
       nspaces = s [5],
       nchrars = s [5] + s [6],
       indentation = indentation)
}

#' Find average amount of leading white space from a vector of all values of
#' white space.
#'
#' This is done by identifying all local maxima, converting their heights to
#' change relative to preceding local maxima, and picking the first value which
#' is higher than all preceding values.
#' @param x The vector representing the frequency table of leading white space
#' values returned by `cpp_loc`
#' @return A single integer of the average leading white space = indentation
#' @noRd
average_leading_white <- function (x) {

    index <- seq_along (leading_white) [-1]

    delta <- rep (0, length (leading_white))
    delta [1] <- leading_white [1] - leading_white [2]
    delta [index] <- leading_white [index] -
        leading_white [index - 1] -
        leading_white [index + 1]
    delta <- c (-1, delta)
    index <- which (delta > 0)
    delta_i <- delta [index]
    delta_ratio <- delta_i [-length (delta_i)] / delta_i [-1]
    # Select first peak which is higher than preceding peak(s):
    i <- which (delta_ratio > 1) [1]

    return (index [i])
}

#' Internal calculation of Lines-of-Code Statistics
#'
#' @param path Directory to package being analysed
#' @return A list of statistics for each of three directories, 'R', 'src', and
#' 'inst/include', each one having 5 statistics of total numbers of lines,
#' numbers of empty lines, total numbers of white spaces, total numbers of
#' characters, and indentation used in files in that directory.
#' 
#' @note NA values are returned for directories which do not exist.
#' @export
loc_stats <- function (path) {

    path <- normalizePath (path)

    dirs <- c ("R", "src", "inst", "tests")
    paths <- file.path (path, dirs)
    paths [3] <- file.path (paths [3], "include")

    dirs <- vapply (strsplit (paths, .Platform$file.sep),
                    function (i) utils::tail (i, 1L),
                    character (1))
    stats <- lapply (dirs, function (i) {
                         res <- loc_stats1 (file.path (path, i))
                         names (res) <- paste0 (i, "_", names (res))
                         return (res)   })
    stats <- unlist (stats)
    names (stats) <- gsub ("^include", "inst", names (stats))

    return (stats)
}

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

    index <- seq_along (x) [-1]

    delta <- rep (0, length (x))
    delta [1] <- x [1] - x [2]
    delta [index] <- x [index] - x [index - 1] - x [index + 1]
    delta <- c (-1, delta)
    index <- which (delta > 0)
    delta_i <- delta [index]
    delta_ratio <- delta_i [-length (delta_i)] / delta_i [-1]
    # Select first peak which is higher than preceding peak(s):
    i <- which (delta_ratio > 1) [1]

    return (index [i])
}

#' Use internal file-type-dict.R to match files in flist to file types
#' @param flist List of files to be analysed
#' @return Modified version of the \link{file_exts} function containing one row
#' for each file in flist, and including also the path to the file.
#' @noRd
get_file_types <- function (flist) {

    exts <- vapply (
        flist, function (i) {
            utils::tail (strsplit (i, "\\.") [[1]], 1)
        },
        character (1)
    )
    ftypes <- file_exts (exts)

    # any non-parseable files (like .o, .so) then have NA extensions:
    index <- which (!is.na (ftypes$ext))
    ftypes <- ftypes [index, ]
    flist <- flist [index]

    # Remove regex-"^\\s*" from start of single-line comments
    ftypes$cmt <- gsub ("\\^\\\\s\\*", "", ftypes$cmt)
    # And reduce all comment symbols to actual symbols minus
    # regex-formatting:
    ftypes$cmt_open <- gsub ("\\\\", "", ftypes$cmt_open)
    ftypes$cmt_close <- gsub ("\\\\", "", ftypes$cmt_close)
    ftypes$cmt <- gsub ("\\\\", "", ftypes$cmt)

    ftypes$file <- flist
    rownames (ftypes) <- NULL

    return (ftypes)
}

#' Internal calculation of Lines-of-Code Statistics
#'
#' @param path Directory to source code of package being analysed
#' @return A list of statistics for each of three directories, 'R', 'src', and
#' 'inst/include', each one having 5 statistics of total numbers of lines,
#' numbers of empty lines, total numbers of white spaces, total numbers of
#' characters, and indentation used in files in that directory.
#'
#' @note NA values are returned for directories which do not exist.
#' @family stats
#' @export
#' @examples
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' # have to extract tarball to call function on source code:
#' path <- extract_tarball (f)
#' loc_stats (path)
loc_stats <- function (path) {

    path <- expand_path (path)

    dirs <- c ("R", "src", "inst", "tests", "vignettes")
    paths <- fs::path (path, dirs)
    paths <- paths [which (fs::dir_exists (paths))]

    flist <- fs::dir_ls (paths, recurse = TRUE)
    # .Rmd files generally excluded except in vignettes:
    rmd_vignettes <- grep ("vignettes.*\\.Rmd$", flist, value = TRUE)
    flist <- flist [which (!grepl (excluded_file_ptn (), flist))]
    flist <- c (flist, rmd_vignettes)

    ftypes <- get_file_types (flist)

    fdirs <- gsub (paste0 (path, .Platform$file.sep), "", ftypes$file)
    fdirs <- vapply (strsplit (fdirs, .Platform$file.sep), function (i) {
        paste0 (i [-length (i)], collapse = .Platform$file.sep)
    },
    character (1),
    USE.NAMES = FALSE
    )

    s <- cpp_loc (
        ftypes$file,
        ftypes$cmt_open,
        ftypes$cmt_close,
        ftypes$cmt
    )

    nstats <- 8L # number of stats for each file, taken from src/loc.cpp

    index <- seq_along (s) [-(seq (nstats * nrow (ftypes)))]
    leading_white <- s [index [-1]] # rm 1st value
    indentation <- average_leading_white (leading_white)

    index <- seq (nstats * nrow (ftypes))
    s <- data.frame (
        t (matrix (s [index], nrow = nstats)),
        stringsAsFactors = FALSE
    )
    names (s) <- c (
        "nlines",
        "ncode",
        "ndoc",
        "nempty",
        "nspaces",
        "nchars",
        "nbrackets",
        "ntabs"
    )

    s$nbrackets [s$nbrackets < 1] <- NA_integer_
    s$language <- s$dir <- ""
    if (nrow (ftypes) > 0) { # data pkgs may have no code
        s$language <- ftypes$type
        s$dir <- fdirs
    }

    # suprress no visible binding notes:
    language <- nfiles <- nlines <- ncode <- ndoc <-
        nempty <- nspaces <- nchars <- nbrackets <- ntabs <- NULL

    # No magrittr here, plus note final renaming of nbrackets to nexpr
    xg <- dplyr::group_by (s, language, dir)
    s <- dplyr::summarise (
        xg,
        nfiles = length (which (nlines > 0)),
        nlines = sum (nlines),
        ncode = sum (ncode),
        ndoc = sum (ndoc),
        nempty = sum (nempty),
        nspaces = sum (nspaces),
        nchars = sum (nchars),
        nexpr = stats::median (nbrackets, na.rm = TRUE),
        ntabs = sum (ntabs),
        .groups = "keep"
    )
    s$indentation <- indentation

    return (s)
}

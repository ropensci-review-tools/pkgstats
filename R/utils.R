
get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools") # nolint

#' Check path is an existing directory
#' @noRd
check_path <- function (path) {

    if (!file.exists (path))
        stop (paste0 ("path [", path, "] does not exist. ",
                      "Did you first 'extract_tarball()'?"))
}

#' Decompose file paths into character vectors of named directories and final
#' file names
#'
#' @param f One of more file paths with system-dependent file separators
#' @return List of equivalent character vectors from which paths can be
#' reconstructed with \link{file.path}
#' @noRd
decompose_path <- function (f) {

    # https://github.com/r-lib/fs/blob/4cc4b56c26b9d7f177a676fbb331133bb2584b86/R/path.R
    strsplit (f, "^(?=/)(?!//)|(?<!^)(?<!^/)/", perl = TRUE)
}

#' Error-controlled version of parse
#'
#' `parse` can fail even with enforced encoding because of multi-byte
#' characters. This is a hacky work-around to find and simply strip any such
#' characters, on the assumption that these can't actually be part of any
#' language expressions.
#'
#' @param file name of R source file
#' @return contents of `parse(file)`
#' @noRd
control_parse <- function (file) {

    # use "latin1" encoding to force re-coding of any non-latin characters:
    x <- readr::read_lines (file,
                            locale = readr::locale (encoding = "latin1"),
                            progress = FALSE)

    # note: keep.source must be TRUE as it is, for example, switched off in
    # `rmarkdown` environments, which means no parse data are returned by
    # getParseData.
    out <- tryCatch (parse (text = x, keep.source = TRUE, encoding = "UTF-8"),
                     error = function (e) e)

    count <- 0
    while (methods::is (out, "simpleError") &
           count < floor (length (x) / 10)) {

        g <- gregexpr ("\'.*\'", out$message)
        ptn <- gsub ("\'", "", regmatches (out$message, g) [[1]])
        if (substring (ptn, 1, 1) == "\\" & substring (ptn, 2, 2) != "\\") {
            ptn <- paste0 ("\\", ptn)
        }

        # Then find line to implement substitution:
        g <- gregexpr ("\\\"", out$message) [[1]]
        expr <- gsub ("\"", "", substring (out$message, min (g), max (g)))
        if (grepl ("\\\\", expr))
            expr <- gsub ("\\", "\\\\", expr, fixed = TRUE)
        index <- which (grepl (ptn, x) & grepl (expr, x))
        x [index] <- gsub (ptn, "", x [index])

        out <- tryCatch (parse (text = x, keep.source = TRUE, encoding = "UTF-8"),
                         error = function (e) e)

        count <- count + 1L
    }

    return (out)
}

#' Function to remove select kinds of files from analyses
#'
#' Currently only used to remove '.min.js' files, under the assumption that
#' these almost always reflect bundled code from external sources. Their
#' inclusion also tends to skew metrics, particularly those related to spacing.
#'
#' @param flist Result of a `list.files` command
#' @return Potentially shortened version with some files excluded
#' @noRd
remove_excluded_files <- function (flist) {

    exclude_ptn <- "\\.min\\.js$"
    
    return (flist [which (!grepl (exclude_ptn, flist))])
}

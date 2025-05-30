get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools") # nolint

#' Check path is an existing root directory of an R package
#'
#' Uses `rprojroot::is_r_package` criterion:
#' "contains a file "DESCRIPTION" with contents matching "^Package: "
#' @noRd
check_path <- function (path) {

    path <- fs::path_real (path)

    if (grepl ("\\.tar\\.gz$", path)) {

        checkmate::assert_file_exists (path)

    } else {

        checkmate::assert_directory_exists (path)

        count <- 1L
        flist <- basename (fs::dir_ls (path))
        while (!"DESCRIPTION" %in% flist && count < 5L) {

            path <- fs::path_real (fs::path (path, ".."))
            flist <- basename (fs::dir_ls (path))
            count <- count + 1L
        }

        desc <- fs::dir_ls (
            path,
            regexp = "DESCRIPTION"
        )
        if (length (desc) == 0L) {
            stop ("Path does not correspond to an R package")
        }
        desc <- brio::read_lines (desc)
        if (!any (grepl ("^Package:\\s", desc))) {
            stop ("Path does not correspond to an R package")
        }
    }

    return (path)
}

#' Expand path
#'
#' The `fs::path_real()` function does the same direct expansion, but requires
#' that path to exist. This version expands both "~" and "." without requiring
#' that full paths necessarily exist. 'fs' has a `path_expand()` function which
#' expands "~" only, but not ".". This function reverses the name of that one,
#' to avoid confusion.
#'
#' @param path Vector of one or more paths
#' @return Expanded, absolute versions of input argument.
#' @noRd
expand_path <- function (path) {
    fs::path_abs (fs::path_expand (path))
}

#' Decompose file paths into character vectors of named directories and final
#' file names
#'
#' @param f One of more file paths with system-dependent file separators
#' @return List of equivalent character vectors from which paths can be
#' reconstructed with \link{file.path}
#' @noRd
decompose_path <- function (f) {

    # https://github.com/r-lib/fs/blob/4cc4b56c26b9d7f177a676fbb331133bb2584b86/R/path.R # nolint
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
        progress = FALSE
    )

    # note: keep.source must be TRUE as it is, for example, switched off in
    # `rmarkdown` environments, which means no parse data are returned by
    # getParseData.
    out <- tryCatch (parse (text = x, keep.source = TRUE, encoding = "UTF-8"),
        error = function (e) e
    )

    count <- 0
    nchars0 <- sum (nchar (x))
    while (methods::is (out, "simpleError") &&
        count < floor (length (x) / 10)) {

        g <- gregexpr ("\'.*\'", out$message)
        ptn <- gsub ("\'", "", regmatches (out$message, g) [[1]])
        if (length (ptn) == 0L) {
            count <- length (x)
            next
        }

        if (substring (ptn, 1, 1) == "\\" && substring (ptn, 2, 2) != "\\") {
            ptn <- paste0 ("\\", ptn)
        }

        # Then find line to implement substitution:
        g <- gregexpr ("\\\"", out$message) [[1]]
        expr <- gsub ("\"", "", substring (out$message, min (g), max (g)))
        if (grepl ("\\\\", expr)) {
            expr <- gsub ("\\", "\\\\", expr, fixed = TRUE)
        }
        index <- which (grepl (ptn, x, fixed = TRUE) &
            grepl (expr, x, fixed = TRUE))
        x [index] <- gsub (ptn, "", x [index], fixed = TRUE)

        nchars <- sum (nchar (x))

        out <- tryCatch (
            parse (text = x, keep.source = TRUE, encoding = "UTF-8"),
            error = function (e) e
        )

        if (nchars == nchars0) {
            count <- length (x)
        } else {
            count <- count + 1L
            nchars0 <- nchars
        }
    }

    return (out)
}

#' Regex to remove select kinds of files from analyses
#'
#' These file types are excluded from `loc_stats`, as well as from both `ctags`
#' and `gtags` analyses.
#' @return regex pattern
#' @noRd
excluded_file_ptn <- function () {

    exts <- c (
        "h", "rda", "rds", "Rd", "md", "Rmd", "win", "min.js",
        "png", "svg", "jpg", "gif"
    )
    paste0 ("(", paste0 ("\\.", exts, collapse = "|"), ")$")
}

which_unix <- function () {

    if (!.Platform [["OS.type"]] == "unix") {
        return (NULL)
    }

    x <- utils::capture.output (
        sys::exec_wait ("lsb_release", args = "-a", std_out = TRUE)
    )
    strsplit (grep ("^Distributor ID", x, value = TRUE), "\\t") [[1]] [2]
}

rm_file_no_err <- function (f) {

    tryCatch (
        file.remove (f),
        error = function (e) NULL
    )
}

sys_is_linux <- function () {
    tolower (Sys.info () [["sysname"]]) == "linux"
}

get_processes <- function () {

    if (!sys_is_linux ()) {
        return ()
    }

    p <- system ("ps aux", intern = TRUE)
    # It's fixed-width as defined by header:
    index <- gregexpr ("[^[:space:]]", p [1]) [[1]]
    index <- index [-(which (diff (index) == 1) + 1)]
    index <- cbind (index, c (index [-1] - 1, nchar (p [1])))

    p <- apply (index, 1, function (i) substring (p, i [1], i [2]))
    p <- data.frame (p, stringsAsFactors = FALSE)
    names (p) <- p [1, ]
    p <- p [-1, ]

    return (p)
}

wait_for_process <- function (what = "ctags") {

    if (!sys_is_linux ()) {
        Sys.sleep (0.2)
        return ()
    }

    p <- get_processes ()
    count <- 1
    while (any (grep (what, p$COMMAND))) {
        Sys.sleep (0.2)
        p <- get_processes ()
        count <- count + 1
        if (count > 10) {
            break
        }
    }
}

#' test a 'ctags' installation
#'
#' This uses the example from
#' \url{https://github.com/universal-ctags/ctags/blob/master/man/ctags-lang-r.7.rst.in}
#' and also checks the GNU global installation.
#' @param quiet If `TRUE`, display on screen whether or not 'ctags' is correctly
#' installed.
#' @param noerror If `FALSE` (default), this function will error if either
#' 'ctags' or 'gtags' are not installed. If `TRUE`, the function will complete
#' without erroring, and issue appropriate messages regarding required but
#' non-installed system libraries.
#' @return 'TRUE' or 'FALSE' respectively indicating whether or not 'ctags' is
#' correctly installed.
#' @family tags
#' @examples
#' # The function errors if not ctags or gtags found.
#' \donttest{
#' ctags_okay <- !is.null (tryCatch (
#'     ctags_test (),
#'     error = function (e) NULL
#' ))
#' }
#' @export
ctags_test <- function (quiet = TRUE, noerror = FALSE) {
    if (!has_ctags ()) {
        if (noerror) {
            message ("No ctags installation found.")
            return (FALSE)
        } else {
            stop ("No ctags installation found.", call. = FALSE)
        }
    }
    if (!has_gtags ()) {
        if (noerror) {
            message ("No GNU global installation found.")
            return (FALSE)
        } else {
            stop ("No GNU global installation found.", call. = FALSE)
        }
    }

    f_in <- tempfile (fileext = ".R")

    x <- c (
        "G <- 1", # globalVar
        "v <- c(1, 2)", # vector
        "l <- list(3, 4)", # list
        "d <- data.frame(n = v)", # dataframe + nameattr
        "f <- function(a) {", # function
        "    g <- function (b) a + b", # function
        "    w <- c(1, 2)", # vector
        "    m <- list (3, 4)", # list
        "    e <- data.frame(n = w)", # dataframe + nameattr
        "    L <- 2", # functionVar
        "}"
    )
    brio::write_lines (x, path = f_in)

    f_out <- fs::file_temp (ext = ".txt")
    cmd <- paste0 ("ctags --sort=no --fields=+KZ -f ", f_out, " ", f_in)
    system (cmd)

    # remove header lines:
    x <- brio::read_lines (f_out)
    x <- x [-which (grepl ("^\\!", x))]
    brio::write_lines (x, path = f_out)

    ctypes <- list (
        readr::col_character (),
        readr::col_character (),
        readr::col_character (),
        readr::col_character (),
        readr::col_character (),
        readr::col_character (),
        readr::col_character ()
    )
    cnames <- c ("tag", "file", "content", "kind", "start", "language", "end")

    suppressWarnings (
        tags <- readr::read_tsv (f_out,
            col_names = cnames,
            col_types = ctypes,
            col_select = cnames,
            progress = FALSE,
            lazy = FALSE
        )
    )

    fs::file_delete (c (f_in, f_out))

    expected_kinds <- c (
        "globalVar",
        "list",
        "dataframe",
        "nameattr",
        "function",
        "function",
        "vector",
        "list",
        "dataframe",
        "nameattr",
        "functionVar"
    )

    ctags_check <- nrow (tags) == length (expected_kinds)
    if (ctags_check) {
        ctags_check <- all (tags$kind == expected_kinds)
    }

    td <- fs::path (fs::path_temp (), "pkgstats-gtags-test")

    if (fs::dir_exists (td)) {
        gtags_check <- TRUE
    } else {
        f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")

        pkgstats_path <- extract_tarball (f)
        if (fs::dir_exists (td)) {
            fs::dir_delete (td)
        }
        if (fs::file_exists (td)) {
            chk <- fs::file_delete (td)
        }
        fs::dir_create (td, recurse = TRUE)
        chk <- fs::dir_copy (pkgstats_path, td)
        # unlink (pkgstats_path, recursive = TRUE) # done in unload
        if (.Platform$OS.type == "windows") {
            cmd <- "set GTAGS_LABEL=new-ctags; gtags"
            gtags_test <- withr::with_dir (fs::path (td, "pkgstats"), shell (cmd, intern = TRUE))
        } else {
            cmd <- "export GTAGS_LABEL=new-ctags; gtags"
            gtags_test <- withr::with_dir (fs::path (td, "pkgstats"), system (cmd, intern = TRUE))
        }
        gtags_check <- length (gtags_test) == 0L
        if (!gtags_check) {
            gtags_check <- any (grepl ("error", gtags_test, ignore.case = TRUE))
        }

        fs::file_delete (td)
    }

    check <- ctags_check && gtags_check

    if (!check) {
        if (!quiet) {
            message (
                "ctags does not function as required; you may need to upgrade? see\n", # nolint
                "https://github.com/universal-ctags/ctags/blob/master/man/ctags-lang-r.7.rst.in\n", # nolint
                "for expected output"
            )
        }
        ret <- FALSE
    } else {
        if (!quiet) {
            message ("ctags installation works as expected")
        }
        ret <- TRUE
    }

    return (ret)
}

has_ctags <- function () {

    ctags_path <- dirname (Sys.which ("ctags"))
    nzchar (ctags_path)
}

#' ctags has to be "Universal" and not "Exuberant" (both currently v5.9)
#' @noRd
which_ctags <- function () {

    f <- fs::file_temp (pattern = "ctags-out-", ext = ".txt")
    sys::exec_wait ("ctags", args = "--version", std_out = f)
    which_ctags <- brio::read_lines (f)
    regmatches (which_ctags [1], regexpr ("\\w+", which_ctags [1]))
}

#' universal is required, so this should be TRUE
#' @noRd
ctags_is_universal <- function () {

    which_ctags () == "Universal"
}

#' exuberant is the standard apt install, and insufficient here, so this should
#' be FALSE.
#' @noRd
ctags_is_exuberant <- function () {

    which_ctags () == "Exuberant"
}

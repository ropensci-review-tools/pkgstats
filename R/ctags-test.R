#' test a 'ctags' installation
#'
#' This uses the example from
#' \url{https://github.com/universal-ctags/ctags/blob/master/man/ctags-lang-r.7.rst.in}
#' and also checks the GNU global installation.
#' @param quiet If `TRUE`, display on screen whether or not 'ctags' is correctly
#' installed.
#' @return 'TRUE' or 'FALSE' respectively indicating whether or not 'ctags' is
#' correctly installed.
#' @family tags
#' @examples
#' \dontrun{
#' ctags_test ()
#' }
#' @export
ctags_test <- function (quiet = TRUE) {

    if (!has_ctags ()) {
        stop ("No ctags installation found.", call. = FALSE)
    }
    if (!has_gtags ()) {
        stop ("No GNU global installation found.", call. = FALSE)
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

    f_out <- tempfile (fileext = ".txt")
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

    file.remove (c (f_in, f_out))

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

    td <- file.path (tempdir (), "pkgstats-gtags-test")
    if (dir.exists (td)) {
        gtags_check <- TRUE
    } else {
        f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
        pkgstats_path <- extract_tarball (f)
        if (dir.exists (td)) {
            chk <- unlink (td, recursive = TRUE)
        }
        if (file.exists (td)) {
            chk <- file.remove (td)
        }
        dir.create (td)
        chk <- file.copy (pkgstats_path, td, recursive = TRUE)
        # unlink (pkgstats_path, recursive = TRUE) # done in unload
        gtags_test <- withr::with_envvar (
            c ("GTAGSLABEL" = "new-ctags"),
            withr::with_dir (
                file.path (td, "pkgstats"),
                system2 ("gtags",
                    stdout = TRUE, stderr = TRUE
                )
            )
        )
        gtags_check <- length (gtags_test) == 0L
        if (!gtags_check) {
            gtags_check <- any (grepl ("error", gtags_test, ignore.case = TRUE))
        }

        unlink (td, recursive = TRUE)
    }

    check <- ctags_check & gtags_check

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

    f <- tempfile (pattern = "ctags-out-", fileext = ".txt")
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

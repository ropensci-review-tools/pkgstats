# Mostly only lightly adapted from
# https://github.com/stan-dev/cmdstanr/blob/master/R/install.R

# nocov start
clone_ctags <- function (destdir = NULL) {

    if (!has_git ()) {
        stop ("Ctags can only be installed with git", call. = FALSE)
    }

    if (is.null (destdir)) {
        stop ("destdir must be specified", call. = FALSE)
    }
    destdir <- normalizePath (destdir, mustWork = FALSE)
    if (!dir.exists (destdir)) {
        stop ("Directory [", destdir,
            "] does not exist",
            call. = FALSE
        )
    }

    u <- "https://github.com/universal-ctags/ctags.git"

    withr::with_dir (destdir, {
        sys::exec_wait ("git", args = c ("clone", u))
    })

    return (file.path (destdir, "ctags"))
}

has_git <- function () {

    nzchar (dirname (Sys.which ("git")))
}

#' Make install ctags from extracted .tar.gz in 'ctags_dir', returned from
#' `download_ctags()`.
#' @inheritParams ctags_install
#' @param ctags_dir Directory of 'ctags' repository cloned with
#' \link{clone_ctags}.
#' @noRd
ctags_make <- function (ctags_dir, bin_dir = NULL, sudo = TRUE) {

    if (!sudo & is.null (bin_dir)) {
        stop ("A value for 'bin_dir' must be specified when 'sudo = FALSE'")
    }

    f <- tempfile (pattern = "ctags-make-", fileext = ".txt")

    confargs <- NULL
    if (!is.null (bin_dir)) {
        if (!dir.exists (bin_dir)) {
            stop ("bin_dir [", bin_dir, "] does not exist.", call. = FALSE)
        }
        confargs <- paste0 ("--prefix=", bin_dir)
    }

    if (sudo) {
        cmd <- "sudo"
        arg <- c ("make", "install")
    } else {
        cmd <- "make"
        arg <- "install"
    }

    withr::with_dir (ctags_dir, {
        sys::exec_wait ("./autogen.sh", std_out = f)
        sys::exec_wait ("./configure", args = confargs, std_out = f)
        sys::exec_wait ("make", std_out = f)
        sys::exec_wait (cmd, args = arg, std_out = f)
    })
}

#' Install 'ctags' from a clone of the 'git' repository
#'
#' 'ctags' is installed with this package on both Windows and macOS systems;
#' this is an additional function to install from source on Unix systems.
#'
#' @param bin_dir Prefix to pass to the `autoconf` configure command
#' defining location to install the binary, with default of `/usr/local`.
#' @param sudo Set to `FALSE` if `sudo` is not available, in which case a
#' value for `bin_dir` will also have to be explicitly specified, and be a
#' location where a binary is able to be installed without `sudo` privileges.
#' @return Nothing; the function will fail if installation fails, otherwise
#' returns nothing.
#'
#' @family tags
#' @examples
#' \dontrun{
#' ctags_install (bin_dir = "/usr/local") # default
#' }
#' @export
ctags_install <- function (bin_dir = NULL, sudo = TRUE) {

    if (!.Platform$OS.type == "unix") {
        return (NULL)
    }

    if (tryCatch (ctags_test (),
        error = function (e) FALSE
    )) {
        # already installed and okay
        return (NULL)
    }

    ctags_dir <- clone_ctags (destdir = tempdir ())
    ctags_make (ctags_dir, bin_dir, sudo)

    if (!has_gtags ()) {
        gtags_install (sudo = sudo)
    }

    if (!sudo) {
        # system ("hash -d ctags")
        system ("hash ctags")
        system ("hash gtags")
    }
}
# nocov end

# Mostly only lightly adapted from
# https://github.com/stan-dev/cmdstanr/blob/master/R/install.R

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
              "] does not exist", call. = FALSE)
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
#' @note Requires 'sudo' and will fail if not
#' @param ctags_dir Directory of 'ctags' repository cloned with
#' \link{clone_ctags}.
#' @param bin_dir Prefix to pass to the `autoconf` configure command
#' defining location to install the binary, with default of `/usr/local`.
#' @noRd
ctags_make <- function (ctags_dir, bin_dir = NULL) {

    f <- tempfile (pattern = "ctags-make-", fileext = ".txt")

    confargs <- NULL
    if (!is.null (bin_dir)) {
        if (!dir.exists (bin_dir))
            stop ("bin_dir [", bin_dir, "] does not exist.", call. = FALSE)
        confargs <- paste0 ("--prefix=", bin_dir)
    }

    withr::with_dir (ctags_dir, {
        sys::exec_wait ("./autogen.sh", std_out = f)
        sys::exec_wait ("./configure", args = confargs, std_out = f)
        sys::exec_wait ("make", std_out = f)
        sys::exec_wait ("sudo", args = c ("make", "install"), std_out = f)
        })
}

#' Install 'ctags' from a clone of the 'git' repository
#'
#' 'ctags' is installed with this package on both Windows and macOS systems; this
#' is an additional function to install from source on Unix systems. The
#' installation requires 'sudo' privileges, and will fail otherwise.
#'
#' @param bin_dir Prefix to pass to the `autoconf` configure command
#' defining location to install the binary, with default of `/usr/local`.
#' @export
ctags_install <- function (bin_dir = NULL) {

    if (!.Platform$OS.type == "unix")
        return (NULL)

    if (ctags_test ()) # already installed and okay
        return (NULL)

    ctags_dir <- clone_ctags (destdir = tempdir ())
    ctags_make (ctags_dir, bin_dir)

    if (!has_gtags ())
        gtags_install ()
}

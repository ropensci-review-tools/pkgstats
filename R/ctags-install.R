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
    destdir <- expand_path (destdir)
    if (!fs::dir_exists (destdir)) {
        stop ("Directory [", destdir,
            "] does not exist",
            call. = FALSE
        )
    }

    u <- "https://github.com/universal-ctags/ctags.git"

    withr::with_dir (destdir, {
        sys::exec_wait ("git", args = c ("clone", u))
    })

    return (fs::path (destdir, "ctags"))
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

    if (!sudo && is.null (bin_dir)) {
        stop ("A value for 'bin_dir' must be specified when 'sudo = FALSE'")
    }

    f <- fs::file_temp (pattern = "ctags-make-", ext = ".txt")

    confargs <- NULL
    if (!is.null (bin_dir)) {
        if (!fs::dir_exists (bin_dir)) {
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
#' @param bin_dir This parameter only has an effect on *nix-type operating
#' systems (such as Linux), on which it's a prefix to pass to the
#' `autoconf` configure command defining location to install the binary, with
#' default of `/usr/local`.
#' @param sudo Set to `FALSE` if `sudo` is not available, in which case a
#' value for `bin_dir` will also have to be explicitly specified, and be a
#' location where a binary is able to be installed without `sudo` privileges.
#' @return Nothing; the function will fail if installation fails, otherwise
#' returns nothing.
#'
#' @family tags
#' @examples
#' \dontrun{
#' ctags_install (bin_dir = "/usr/local") # default Linux location.
#' }
#' @export
ctags_install <- function (bin_dir = NULL, sudo = TRUE) {

    pkg_path <- system.file (package = "pkgstats")
    pkg_path <- gsub ("/inst", "", pkg_path)

    os <- Sys.info () ["sysname"]

    if (os == "Darwin") {
        install_ctags_macos (pkg_path)
    }

    if (os == "Windows") {
        install_ctags_windows (pkg_path)
    }

    if (.Platform$OS.type == "unix" & os != "Darwin") {
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

    if (ctags_test (noerror = TRUE)) {
        # already installed and okay
        message ("ctags installed and passed test.")
        return (NULL)
    }
}

install_ctags_macos <- function (pkg_path) {

    ctags_dir <- fs::path (pkg_path, "inst", "bin")
    if (!fs::dir_exists (ctags_dir)) {
        fs::dir_create (ctags_dir, recurse = TRUE)
    }
    ctags_path <- fs::path (ctags_dir, "ctags")
    if (!file.exists (ctags_path)) {
        u <- "https://autobrew.github.io/archive/high_sierra/universal-ctags-p5.9.20210530.0.tar.xz" # nolint
        f <- basename (u)
        utils::download.file (u, f, quiet = TRUE)
        utils::untar (f, extras = "--strip-components=1", exdir = ctags_dir)
        fs::file_delete (f)
    }
}

get_ctags_path_win <- function (pkg_path) {
    u <- "https://github.com/rwinlib/universal-ctags/archive/refs/tags/v5.9.20210530.0.zip" # nolint
    ctags_path <- normalizePath (fs::path (
        pkg_path,
        "windows",
        paste0 (
            "universal-ctags-",
            tools::file_path_sans_ext (gsub ("^v", "", basename (u)))
        ),
        "bin"
    ), mustWork = FALSE)
    ctags_path
}

install_ctags_windows <- function (pkg_path) {
    u <- "https://github.com/rwinlib/universal-ctags/archive/refs/tags/v5.9.20210530.0.zip" # nolint
    ctags_path <- get_ctags_path_win (pkg_path)
    ctag_win_path <- fs::path_abs (fs::path (pkg_path, "windows"))
    if (!fs::dir_exists (ctag_win_path)) {
        fs::dir_create (ctag_win_path, recurse = TRUE)
    }
    if (!fs::file_exists (ctags_path)) {
        f <- tempfile (fileext = "pkgstats_ctags_lib.zip")
        utils::download.file (u, f, quiet = TRUE)
        utils::unzip (f, exdir = ctags_path, junkpaths = TRUE)
        fs::file_delete (f)
    }
    if (!grepl ("universal-ctags-5.9.20210530.0", Sys.getenv ("PATH"))) {
        Sys.setenv (PATH = paste0 (Sys.getenv ("PATH"), ";", ctags_path))
    }
}
# nocov end

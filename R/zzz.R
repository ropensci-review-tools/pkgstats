#' My (@mpadge) contribution to Brian Connely's 'ami' package which got removed
#' from CRAN, and so removed as dependency here.
#'
#' \url{https://github.com/briandconnelly/ami/blob/main/R/cran.R}, with logic
#' of function detailed in
#' \url{https://github.com/briandconnelly/ami/issues/14}
#' @noRd
on_cran <- function (cran_pattern = "_R_", n_cran_envvars = 5L) {

    checkmate::assert_string (cran_pattern)
    checkmate::assert_integer (n_cran_envvars, lower = 1L, len = 1L)

    not_cran <- identical (Sys.getenv ("NOT_CRAN", "false"), "true")
    if (!not_cran) { # Not using the envvar
        index <- grep (cran_pattern, names (Sys.getenv ()), fixed = TRUE)
        not_cran <- length (index) < n_cran_envvars
    }
    !not_cran
}

# nocov start
.onAttach <- function (libname, pkgname) { # nolint

    exit_now <- !interactive () || on_cran ()
    if (exit_now) {
        return ()
    }

    pkg_path <- fs::path_abs (fs::path (libname, pkgname))

    os <- Sys.info () ["sysname"]
    if (os == "Linux") {
        return ()
    }
    has_ctags <- pkgstats::ctags_test ()


    if (os == "Darwin" && !has_ctags) {
        packageStartupMessage (
            "This package requires downloading and ",
            "installing binary 'universal-ctags' software from:"
        )
        packageStartupMessage (
            "https://github.com/autobrew/archive/tree/master/high_sierra"
        )
        packageStartupMessage (
            "Run pkgstats::ctags_install() to install universal-ctags."
        )
    } else if (os == "Windows" && !has_ctags) {
        packageStartupMessage (
            "This package requires downloading and installing ",
            "binary 'universal-ctags' software from:"
        )
        packageStartupMessage ("https://github.com/rwinlib/universal-ctags/")
        packageStartupMessage (
            "Run pkgstats::ctags_install() to install universal-ctags."
        )
    }
}

.onLoad <- function (libname, pkgname) {

    os <- Sys.info () ["sysname"]

    if (os == "Windows") {
        pkg_path <- system.file (package = "pkgstats")
        pkg_path <- gsub ("/inst", "", pkg_path)
        ctags_path <- get_ctags_path_win (pkg_path)
        if (!grepl ("universal-ctags-5.9.20210530.0", Sys.getenv ("PATH"))) {
            Sys.setenv (PATH = paste0 (Sys.getenv ("PATH"), ";", ctags_path))
        }
    }

}
# nocov end

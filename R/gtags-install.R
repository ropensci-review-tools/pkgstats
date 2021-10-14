
# nocov start
has_gtags <- function () {

    gtags_path <- dirname (Sys.which ("gtags"))
    nzchar (gtags_path)
}

#' Install gtags on a unix system (currently only Ubuntu + Arch).
#' @note Requires sudo
#' @noRd
gtags_install <- function () {

    if (has_gtags ())
        return (NULL)

    if (!.Platform$OS.type == "unix")
        return (NULL)

    f <- tempfile (pattern = "gtags-install-", fileext = ".txt")

    unix <- which_unix ()
    arg <- c ("apt-get", "install", "-y")
    if (unix == "Arch") {
        arg <- c ("pacman", "-Syu")
    }

    sys::exec_wait ("sudo", args = c (arg, "global"),
                    std_out = f)
}
# nocov end

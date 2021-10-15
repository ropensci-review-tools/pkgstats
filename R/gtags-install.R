
# nocov start
has_gtags <- function () {

    gtags_path <- dirname (Sys.which ("gtags"))
    nzchar (gtags_path)
}

#' Install gtags on a unix system (currently only Ubuntu + Arch).
#' @param sudo Set to `FALSE` if `sudo` is not available.
#' @noRd
gtags_install <- function (sudo = TRUE) {

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
    if (sudo) {
        cmd <- "sudo"
    } else {
        cmd <- arg [1]
        arg <- arg [-1]
    }

    sys::exec_wait (cmd, args = c (arg, "global"), std = f)
}
# nocov end

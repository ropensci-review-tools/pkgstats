# nocov start
has_gtags <- function () {

    gtags_path <- dirname (Sys.which ("gtags"))
    nzchar (gtags_path)
}

#' Install gtags on a unix system (currently only Ubuntu + Arch).
#' @param sudo Set to `FALSE` if `sudo` is not available.
#' @noRd
gtags_install <- function (sudo = TRUE) {

    if (has_gtags ()) {
        return (NULL)
    }

    if (!.Platform$OS.type == "unix") {
        return (NULL)
    }

    f <- fs::file_temp (pattern = "gtags-install-", ext = ".txt")

    unix <- which_unix ()

    if (sudo) {

        cmd <- "sudo"
        arg <- c ("apt-get", "install", "-y")
        if (unix == "Arch") {
            arg <- c ("pacman", "-Syu")
        }

        sys::exec_wait (cmd, args = c (arg, "global"), std_out = f)
    } else {
        gtags_compile ()
    }
}

gtags_compile <- function () {

    u <- "https://ftp.gnu.org/pub/gnu/global/global-6.6.8.tar.gz"
    f <- fs::path (fs::path_temp (), basename (u))
    utils::download.file (u, f)
    utils::untar (f, exdir = fs::path_temp ())

    home <- fs::path_expand ("~")
    conf_args <- c ("--prefix", home, "--disable-gtagscscope")
    withr::with_dir (fs::path (fs::path_temp (), "global-6.6.8"), {
        sys::exec_wait ("./configure", args = conf_args)
        sys::exec_wait ("make")
        sys::exec_wait ("make", args = "install")
        sys::exec_wait ("hash", args = c (
            "-p",
            fs::path (home, "bin", "ctags"),
            "ctags"
        ))
    })
}
# nocov end

# nocov start
.onAttach <- function (libname, pkgname) { # nolint

    if (!interactive () || ami::on_cran ()) {
        return ()
    }

    pkg_path <- fs::path_abs (fs::path (libname, pkgname))

    os <- Sys.info () ["sysname"]

    if (os == "Darwin") {
        packageStartupMessage (
            "This package requires downloading and ",
            "installing binary 'universal-ctags' software from:"
        )
        packageStartupMessage (
            "https://github.com/autobrew/archive/tree/master/high_sierra"
        )
        chk <- readline ("Do you agree (y/n)?")
        if (substring (tolower (chk), 1, 1) == "y") {
            install_ctags_macos (pkg_path)
        }
    } else if (os == "Windows") {
        packageStartupMessage (
            "This package requires downloading and installing ",
            "binary 'universal-ctags' software from:"
        )
        packageStartupMessage ("https://github.com/rwinlib/universal-ctags/")
        chk <- readline ("Do you agree (y/n)?")
        if (substring (tolower (chk), 1, 1) == "y") {
            install_ctags_windows (pkg_path)
        }
        ctags_path <- get_ctags_path(pkg_path)
        Sys.setenv(PATH = paste0(Sys.getenv("PATH"), ";", ctags_path))
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

get_ctags_path <- function(pkg_path){
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
    ctags_path <- get_ctags_path(pkg_path)
    ctag_win_path <- fs::path_abs (fs::path (pkg_path, "windows"))
    if (!fs::dir_exists (ctag_win_path)) {
        fs::dir_create (ctag_win_path, recurse = TRUE)
    }
    if (!fs::file_exists (ctags_path)) {
        f <- "lib.zip"
        utils::download.file (u, f, quiet = TRUE, mode = "wb")
        utils::unzip (f, exdir = ctag_win_path)
        fs::file_delete (f)
    }
}
# nocov end

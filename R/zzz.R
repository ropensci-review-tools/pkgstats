# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    if (!interactive () || ami::on_cran ()) {
        return ()
    }

    pkg_path <- normalizePath (file.path (libname, pkgname), mustWork = FALSE)

    os <- Sys.info () ["sysname"]

    if (os == "Darwin") {
        install_ctags_macos (pkg_path)
    } else if (os == "Windows") {
        install_ctags_windows (pkg_path)
    }
}

install_ctags_macos <- function (pkg_path) {

    ctags_dir <- file.path (pkg_path, "inst", "bin")
    if (!dir.exists (ctags_dir)) {
        dir.create (ctags_dir, recursive = TRUE)
    }
    ctags_path <- file.path (ctags_dir, "ctags")
    if (!file.exists (ctags_path)) {
        u <- "https://autobrew.github.io/archive/high_sierra/universal-ctags-p5.9.20210530.0.tar.xz" # nolint
        f <- basename (u)
        utils::download.file (u, f, quiet = TRUE)
        utils::untar (f, extras = "--strip-components=1", exdir = ctags_dir)
        unlink (f)
    }
}

install_ctags_windows <- function (pkg_path) {
    u <- "https://github.com/rwinlib/universal-ctags/archive/refs/tags/v5.9.20210530.0.zip" # nolint
    ctags_path <- normalizePath (file.path (
        "windows",
        paste0 ("universal-ctags-", tools::file_path_sans_ext (gsub ("^v", "", basename (u)))),
        "bin"
    ))
    if (!dir.exists ("windows")) {
        dir.create ("windows", recursive = TRUE, showWarnings = FALSE)
    }
    if (!file.exists (ctags_path)) {
        utils::download.file (u, "lib.zip", quiet = TRUE)
        utils::unzip ("lib.zip", exdir = "windows")
        unlink ("lib.zip")
    }
}
# nocov end

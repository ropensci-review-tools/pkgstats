# Add pkg executable directory to the path
.onLoad <- function (libname, pkgname) { # nolint

    bin <- expand_path (fs::path (libname, pkgname, "bin"))
    Sys.setenv (
        PATH = paste (bin, Sys.getenv ("PATH"), sep = .Platform$path.sep)
    )
}

.onUnload <- function (libname, pkgname) { # nolint

    d <- fs::path (fs::path_temp (), "pkgstats-gtags-test")
    if (fs::dir_exists (d)) {
        fs::file_delete (d)
    }
    d <- fs::path (fs::path_temp (), "pkgstats")
    if (fs::dir_exists (d)) {
        fs::file_delete (d)
    }
}

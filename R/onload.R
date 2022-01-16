# Add pkg executable directory to the path
.onLoad <- function (libname, pkgname) { # nolint

    bin <- normalizePath (file.path (libname, pkgname, "bin"), mustWork = FALSE)
    Sys.setenv (PATH = paste (bin, Sys.getenv ("PATH"), sep = .Platform$path.sep))
}

.onUnload <- function (libname, pkgname) { # nolint

    d <- file.path (tempdir (), "pkgstats-gtags-test")
    if (dir.exists (d)) {
        chk <- unlink (d, recursive = TRUE)
    }
    d <- file.path (tempdir (), "pkgstats")
    if (dir.exists (d)) {
        chk <- unlink (d, recursive = TRUE)
    }
}

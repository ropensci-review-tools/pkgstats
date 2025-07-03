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
        packageStartupMessage (
            "Run pkgstats::ctags_install() to install universal-ctags."
        )
    } else if (os == "Windows") {
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

# nocov end

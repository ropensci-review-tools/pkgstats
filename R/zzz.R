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

.onLoad <- function(libname, pkgname) {
  
  os <- Sys.info () ["sysname"]
  
  if (os == "Windows") {
    pkg_path <- system.file(package = "pkgstats")
    pkg_path <- gsub("/inst", "", pkg_path)
    ctags_path <- get_ctags_path_win(pkg_path)
    if(!grepl("universal-ctags-5.9.20210530.0", Sys.getenv("PATH"))){
      Sys.setenv(PATH = paste0(Sys.getenv("PATH"), ";", ctags_path))
    }
  }
  
}
# nocov end

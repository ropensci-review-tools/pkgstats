# Add pkg executable directory to the path
.onLoad <- function(libname, pkgname){
  bin <- normalizePath(file.path(libname, pkgname, 'bin'), mustWork = FALSE)
  Sys.setenv(PATH = paste(bin, Sys.getenv('PATH'), sep = .Platform$path.sep))
}

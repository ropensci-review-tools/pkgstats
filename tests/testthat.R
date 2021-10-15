library (testthat)
library (pkgstats)

chk_ctags <- nzchar (dirname (Sys.which ("ctags")))
chk_gtags <- nzchar (dirname (Sys.which ("gtags")))
if (!(chk_ctags & chk_gtags)) {
    ctags_install () # only has effect on unix systems
}

test_check ("pkgstats")

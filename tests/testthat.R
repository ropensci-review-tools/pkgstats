library(testthat)
library(pkgstats)

if (!(has_ctags () & has_gtags ())) {
    ctags_install () # only has effect on unix systems
}

test_check("pkgstats")

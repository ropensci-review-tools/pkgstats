# CRAN notes for pkgstats_0.0.4 submission

This is a first submission of a package for static code analysis of R packages. The submission does not yet have an accompanying DOI because the associated manuscript first requires the package to be published on CRAN. Our next submission will include the DOI.

The package has been checked on all environments listed below, and generates only the single note identifying the package as a new submission.

## Test environments

GitHub actions:
* Linux: R-release, R-devel, R-oldrelease
* OSX: R-release
* Windows: R3.6, R4.0, R-devel

CRAN win-builder:
* R-oldrelease, R-release, R-devel

Package also checked using `Clang++ -Weverything`, and both local memory sanitzer and `rocker/r-devel-san` with clean results.

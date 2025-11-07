# CRAN notes for pkgstats_0.2.1 submission

This is a re-submission of a previously archived package. The previous version downloaded binary data on install. This submission no longer does that, and moves binary downloads to interactive and informative functions, with full explanations of what is downloaded and where.

The submission also currently generates one note regarding a possibly invalid URL. This is, however, for Wikipedia, and generates a "429: Too Many Requests" error on win-builder machines. The URL is valid.


## Test environments

The package has been checked on all environments listed below, and generates only the single note identifying the package as a new submission.

GitHub actions:
* Linux: R-release, R-devel, R-oldrelease
* OSX: R-release
* Windows: R-release, R-devel, R-oldrelease

CRAN win-builder:
* R-oldrelease, R-release, R-devel

Package also checked using `Clang++ -Weverything`, and both local memory sanitzer and `rocker/r-devel-san` with clean results.

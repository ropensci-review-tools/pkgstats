# CRAN notes for pkgstats_0.2.1 submission

This is a re-submission of a previously archived package. The previous version downloaded binary data on install. This submission no longer does that, and moves binary downloads to interactive and informative functions, with full explanations of what is downloaded and where.

A first submission attempt prompted requests to fix the following:

1. Document return values of all functions, which has been done.
2. Replace `dontrun` with `donttest` in examples. This has been done where possible, with almost all other cases made conditional with `dontshow`. There is nevertheless one remaining example which require `dontrun`, in a function which installs a system library.
3. Do not write by default in user's home filespace. The package README file previously wrote only with a condition `identical(Sys.getenv("IN_PKGDOWN"), "true")`, and installed a required system library when that was true. That has now been de-activated, and the entire README pre-rendered with no installation, and therefore no writing anywhere other than R's `tempdir()`.

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

# CRAN notes for pkgstats_0.2.3 submission

This submission is in response to an email regarding hard dependency on 'ami' package. That has now been moved to a soft dependency by putting 'ami' in 'Suggests' only, and skipping gracefully if `requireNamespace('ami')` fails.

Current submission generates no notes, warnings, or errors on any CRAN machines.

This submission also currently generates one note on some win-builder machines regarding a possibly invalid URL. This is, however, for Wikipedia, through a "429: Too Many Requests" error. The URL is valid.


## Test environments

The package has been checked on all environments listed below, and generates only the single note identifying the package as a new submission.

GitHub actions:
* Linux: R-release, R-devel, R-oldrelease
* OSX: R-release
* Windows: R-release, R-devel, R-oldrelease

CRAN win-builder:
* R-oldrelease, R-release, R-devel

Package also checked using `Clang++ -Weverything`, and both local memory sanitzer and `rocker/r-devel-san` with clean results.

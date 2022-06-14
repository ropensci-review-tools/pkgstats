# CRAN notes for pkgstats_0.0.5 submission

This is a first submission of a package for static code analysis of R packages. The submission does not yet have an accompanying DOI because the associated manuscript first requires the package to be published on CRAN. Our next submission will include the DOI.

The following changes have been implemented in response to the helpful comments of Gregor from the previous submission attempt:

1. All functions now specify return values.
2. The "Description" of the DESCRIPTION file has been extended to explain what the package actually does.
3. Former use of 'installed.packages' has been replaced with 'find.package'

Note that the comment on the use of 'setwd()' was likely erroneous. There is indeed an *immediate* call to 'on.exit(setwd(oldwd))', but this is embedded within a two-line call that first unlinks directories temporarily created in the target of the initial `setwd()` call. Those sub-directories are created prior to `setwd()`, and the sequence of the `on.exit()` call thus follows this structure, which I believe to be the appropriate.

## Test environments

The package has been checked on all environments listed below, and generates only the single note identifying the package as a new submission.

GitHub actions:
* Linux: R-release, R-devel, R-oldrelease
* OSX: R-release
* Windows: R-release, R-devel, R-oldrelease

CRAN win-builder:
* R-oldrelease, R-release, R-devel

Package also checked using `Clang++ -Weverything`, and both local memory sanitzer and `rocker/r-devel-san` with clean results.

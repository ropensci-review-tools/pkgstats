<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/pkgstats/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/pkgstats/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropensci-review-tools/pkgstats/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci-review-tools/pkgstats)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/pkgstats)](https://cran.r-project.org/package=pkgstats/)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/pkgstats?color=orange)](https://cran.r-project.org/package=pkgstats)
<!-- badges: end -->

# pkgstats

Extract summary statistics of R package structure and functionality. Not
all statistics of course, but a good go at balancing insightful
statistics while ensuring computational feasibility. `pkgstats` is a
*static* code analysis tool, so is generally very fast (a few seconds at
most for very large packages). Installation is described in [a separate
vignette](https://docs.ropensci.org/pkgstats/articles/installation.html).

## What statistics?

Statistics are derived from these primary sources:

1.  Numbers of lines of code, documentation, and white space (both
    between and within lines) in each directory and language
2.  Summaries of package `DESCRIPTION` file and related package
    meta-statistics
3.  Summaries of all objects created via package code across multiple
    languages and all directories containing source code (`./R`,
    `./src`, and `./inst/include`).
4.  A function call network derived from function definitions obtained
    from [the code tagging library, `ctags`](https://ctags.io), and
    references (“calls”) to those obtained from [another tagging
    library, `gtags`](https://www.gnu.org/software/global/). This
    network roughly connects every object making a call (as `from`) with
    every object being called (`to`).
5.  An additional function call network connecting calls within R
    functions to all functions from other R packages.

The [primary function,
`pkgstats()`](https://docs.ropensci.org/pkgstats/reference/pkgstats.html),
returns a list of these various components, including full `data.frame`
objects for the final three components described above. The statistical
properties of this list can be aggregated by the [`pkgstats_summary()`
function](https://docs.ropensci.org/pkgstats/reference/pkgstats_summary.html),
which returns a `data.frame` with a single row of summary statistics.
This function is demonstrated below, including full details of all
statistics extracted.

## Demonstration

The following code demonstrates the output of the main function,
`pkgstats`, using an internally bundled `.tar.gz` “tarball” of this
package. The `system.time` call demonstrates that the static code
analyses of `pkgstats` are generally very fast.

``` r
library (pkgstats)
tarball <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
system.time (
    p <- pkgstats (tarball)
)
```

    ##    user  system elapsed 
    ##   1.701   0.124   1.802

``` r
names (p)
```

    ## [1] "loc"            "vignettes"      "data_stats"     "desc"          
    ## [5] "translations"   "objects"        "network"        "external_calls"

The result is a list of various data extracted from the code. All except
for `objects` and `network` represent summary data:

``` r
p [!names (p) %in% c ("objects", "network", "external_calls")]
```

    ## $loc
    ## # A tibble: 4 × 12
    ##   langage dir        nfiles nlines ncode  ndoc nempty nspaces nchars nexpr ntabs
    ##   <chr>   <chr>       <int>  <int> <int> <int>  <int>   <int>  <int> <dbl> <int>
    ## 1 C++     src             3    365   277    21     67     933   7002     1     0
    ## 2 R       R              19   3741  2698   536    507   27575  94022     1     0
    ## 3 R       tests           2    146   121     1     24     395   2423     1     0
    ## 4 R       tests/tes…      5    202   145     9     48     375   3738     1     0
    ## # ℹ 1 more variable: indentation <int>
    ## 
    ## $vignettes
    ## vignettes     demos 
    ##         0         0 
    ## 
    ## $data_stats
    ##           n  total_size median_size 
    ##           0           0           0 
    ## 
    ## $desc
    ##    package version                date license
    ## 1 pkgstats     9.9 2022-05-12 19:41:22   GPL-3
    ##                                                                                      urls
    ## 1 https://docs.ropensci.org/pkgstats/,\nhttps://github.com/ropensci-review-tools/pkgstats
    ##                                                       bugs aut ctb fnd rev ths
    ## 1 https://github.com/ropensci-review-tools/pkgstats/issues   1   0   0   0   0
    ##   trl depends                                                        imports
    ## 1   0      NA brio, checkmate, dplyr, fs, igraph, methods, readr, sys, withr
    ##                                                                         suggests
    ## 1 hms, knitr, pbapply, pkgbuild, Rcpp, rmarkdown, roxygen2, testthat, visNetwork
    ##   enhances linking_to
    ## 1       NA      cpp11
    ## 
    ## $translations
    ## [1] NA

The various components of these results are described in further detail
in the [main package
vignette](https://docs.ropensci.org/pkgstats/articles/pkgstats.html).

### Overview of statistics and the `pkgstats_summary()` function

A summary of the `pkgstats` data can be obtained by submitting the
object returned from `pkgstats()` to the [`pkgstats_summary()`
function](https://docs.ropensci.org/pkgstats/reference/pkgstats_summary.html):

``` r
s <- pkgstats_summary (p)
```

This function reduces the result of the [`pkgstats()`
function](https://docs.ropensci.org/pkgstats/reference/pkgstats_summary.html)
to a single line with 95 entries, represented as a `data.frame` with one
row and that number of columns. This format is intended to enable
summary statistics from multiple packages to be aggregated by simply
binding rows together. While 95 statistics might seem like a lot, the
[`pkgstats_summary()`
function](https://docs.ropensci.org/pkgstats/reference/pkgstats_summary.html)
aims to return as many usable raw statistics as possible in order to
flexibly allow higher-level statistics to be derived through combination
and aggregation. These 95 statistics can be roughly grouped into the
following categories (not shown in the order in which they actually
appear), with variable names in parentheses after each description. Some
statistics are summarised as comma-delimited character strings, such as
translations into human languages, or other packages listed under
“depends”, “imports”, or “suggests”. This enables subsequent analyses of
their contents, for example of actual translated languages, or both
aggregate numbers and individual details of all package dependencies, as
demonstrated immediately below.

**Package Summaries**

- name (`package`)
- Package version (`version`)
- Package date, as modification time of `DESCRIPTION` file where not
  explicitly stated (`date`)
- License (`license`)
- Languages, as a single comma-separated character value (`languages`),
  and excluding `R` itself.
- List of translations where package includes translations files, given
  as list of (spoken) language codes (`translations`).

**Information from `DESCRIPTION` file**

- Package URL(s) (`url`)
- URL for BugReports (`bugs`)
- Number of contributors with role of *author* (`desc_n_aut`),
  *contributor* (`desc_n_ctb`), *funder* (`desc_n_fnd`), *reviewer*
  (`desc_n_rev`), *thesis advisor* (`ths`), and *translator* (`trl`,
  relating to translation between computer and not spoken languages).
- Comma-separated character entries for all `depends`, `imports`,
  `suggests`, and `linking_to` packages.

Numbers of entries in each the of the last two kinds of items can be
obtained from by a simple `strsplit` call, like this:

``` r
deps <- strsplit (s$suggests, ", ") [[1]]
length (deps)
print (deps)
```

    ## [1] 9

``` r
print (deps)
```

    ## [1] "hms"        "knitr"      "pbapply"    "pkgbuild"   "Rcpp"      
    ## [6] "rmarkdown"  "roxygen2"   "testthat"   "visNetwork"

**Numbers of files and associated data**

- Number of vignettes (`num_vignettes`)
- Number of demos (`num_demos`)
- Number of data files (`num_data_files`)
- Total size of all package data (`data_size_total`)
- Median size of package data files (`data_size_median`)
- Numbers of files in main sub-directories (`files_R`, `files_src`,
  `files_inst`, `files_vignettes`, `files_tests`), where numbers are
  recursively counted in all sub-directories, and where `inst` only
  counts files in the `inst/include` sub-directory.

**Statistics on lines of code**

- Total lines of code in each sub-directory (`loc_R`, `loc_src`,
  `loc_ins`, `loc_vignettes`, `loc_tests`).
- Total numbers of blank lines in each sub-directory (`blank_lines_R`,
  `blank_lines_src`, `blank_lines_inst`, `blank_lines_vignette`,
  `blank_lines_tests`).
- Total numbers of comment lines in each sub-directory
  (`comment_lines_R`, `comment_lines_src`, `comment_lines_inst`,
  `comment_lines_vignettes`, `comment_lines_tests`).
- Measures of relative white space in each sub-directory (`rel_space_R`,
  `rel_space_src`, `rel_space_inst`, `rel_space_vignettes`,
  `rel_space_tests`), as well as an overall measure for the `R/`,
  `src/`, and `inst/` directories (`rel_space`).
- The number of spaces used to indent code (`indentation`), with values
  of -1 indicating indentation with tab characters.
- The median number of nested expression per line of code, counting only
  those lines which have any expressions (`nexpr`).

**Statistics on individual objects (including functions)**

These statistics all refer to “functions”, but actually represent more
general “objects,” such as global variables or class definitions
(generally from languages other than R), as detailed below.

- Numbers of functions in R (`n_fns_r`)
- Numbers of exported and non-exported R functions (`n_fns_r_exported`,
  `n_fns_r_not_exported`)
- Number of functions (or objects) in other computer languages
  (`n_fns_src`), including functions in both `src` and `inst/include`
  directories.
- Number of functions (or objects) per individual file in R and in all
  other (`src`) directories (`n_fns_per_file_r`, `n_fns_per_file_src`).
- Median and mean numbers of parameters per exported R function
  (`npars_exported_mn`, `npars_exported_md`).
- Mean and median lines of code per function in R and other languages,
  including distinction between exported and non-exported R functions
  (`loc_per_fn_r_mn`, `loc_per_fn_r_md`, `loc_per_fn_r_exp_m`,
  `loc_per_fn_r_exp_md`, `loc_per_fn_r_not_exp_mn`,
  `loc_per_fn_r_not_exp_m`, `loc_per_fn_src_mn`, `loc_per_fn_src_md`).
- Equivalent mean and median numbers of documentation lines per function
  (`doclines_per_fn_exp_mn`, `doclines_per_fn_exp_md`,
  `doclines_per_fn_not_exp_m`, `doclines_per_fn_not_exp_md`,
  `docchars_per_par_exp_mn`, `docchars_per_par_exp_m`).

**Network Statistics**

The full structure of the `network` table is described below, with
summary statistics including:

- Number of edges, including distinction between languages (`n_edges`,
  `n_edges_r`, `n_edges_src`).
- Number of distinct clusters in package network (`n_clusters`).
- Mean and median centrality of all network edges, calculated from both
  directed and undirected representations of network
  (`centrality_dir_mn`, `centrality_dir_md`, `centrality_undir_mn`,
  `centrality_undir_md`).
- Equivalent centrality values excluding edges with centrality of zero
  (`centrality_dir_mn_no0`, `centrality_dir_md_no0`,
  `centrality_undir_mn_no0`, `centrality_undir_md_no`).
- Numbers of terminal edges (`num_terminal_edges_dir`,
  `num_terminal_edges_undir`).
- Summary statistics on node degree (`node_degree_mn`, `node_degree_md`,
  `node_degree_max`)

**External Call Statistics**

The final column in the result of [the `pkgstats_summary()`
function](https://docs.ropensci.org/pkgstats/reference/pkgstats_summary.html)
summarises the `external_calls` object detailing all calls make to
external packages (including to base and recommended packages). This
summary is also represented as a single character string. Each package
lists total numbers of function calls, and total numbers of unique
function calls. Data for each package are separated by a comma, while
data within each package are separated by a colon.

``` r
s$external_calls
```

    ## [1] "base:447:78,brio:7:1,dplyr:7:4,fs:4:2,graphics:10:2,hms:1:1,igraph:3:3,pbapply:1:1,pkgstats:99:60,readr:8:5,stats:16:2,sys:13:1,tools:2:2,utils:10:7,visNetwork:3:2,withr:5:1"

This structure allows numbers of calls to all packages to be readily
extracted with code like the following:

``` r
calls <- do.call (
    rbind,
    strsplit (strsplit (s$external_call, ",") [[1]], ":")
)
calls <- data.frame (
    package = calls [, 1],
    n_total = as.integer (calls [, 2]),
    n_unique = as.integer (calls [, 3])
)
print (calls)
```

    ##       package n_total n_unique
    ## 1        base     447       78
    ## 2        brio       7        1
    ## 3       dplyr       7        4
    ## 4          fs       4        2
    ## 5    graphics      10        2
    ## 6         hms       1        1
    ## 7      igraph       3        3
    ## 8     pbapply       1        1
    ## 9    pkgstats      99       60
    ## 10      readr       8        5
    ## 11      stats      16        2
    ## 12        sys      13        1
    ## 13      tools       2        2
    ## 14      utils      10        7
    ## 15 visNetwork       3        2
    ## 16      withr       5        1

The two numeric columns respectively show the total number of calls made
to each package, and the total number of unique functions used within
those packages. These results provide detailed information on numbers of
calls made to, and functions used from, other R packages, including base
and recommended packages.

Finally, the summary statistics conclude with two further statistics of
`afferent_pkg` and `efferent_pkg`. These are package-internal measures
of [afferent and efferent
couplings](https://en.wikipedia.org/wiki/Software_package_metrics)
between the files of a package. The *afferent* couplings (`ca`) are
numbers of *incoming* calls to each file of a package from functions
defined elsewhere in the package, while the *efferent* couplings (`ce`)
are numbers of *outgoing* calls from each file of a package to functions
defined elsewhere in the package. These can be used to derive a measure
of “internal package instability” as the ratio of efferent to total
coupling (`ce / (ce + ca)`).

There are many other “raw” statistics returned by the main `pkgstats()`
function which are not represented in `pkgstats_summary()`. The [main
package
vignette](https://docs.ropensci.org/pkgstats/articles/pkgstats.html)
provides further detail on the full results.

The following sub-sections provide further detail on the `objects`,
`network`, and `external_call` items, which could be used to extract
additional statistics beyond those described here.

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->

<!-- prettier-ignore-start -->

<!-- markdownlint-disable -->

All contributions to this project are gratefully acknowledged using the
[`allcontributors` package](https://github.com/ropensci/allcontributors)
following the [allcontributors](https://allcontributors.org)
specification. Contributions of any kind are welcome!

### Code

<table>

<tr>

<td align="center">

<a href="https://github.com/mpadge">
<img src="https://avatars.githubusercontent.com/u/6697851?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/commits?author=mpadge">mpadge</a>
</td>

<td align="center">

<a href="https://github.com/jhollist">
<img src="https://avatars.githubusercontent.com/u/5438539?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/commits?author=jhollist">jhollist</a>
</td>

<td align="center">

<a href="https://github.com/jeroen">
<img src="https://avatars.githubusercontent.com/u/216319?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/commits?author=jeroen">jeroen</a>
</td>

<td align="center">

<a href="https://github.com/Bisaloo">
<img src="https://avatars.githubusercontent.com/u/10783929?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/commits?author=Bisaloo">Bisaloo</a>
</td>

<td align="center">

<a href="https://github.com/thomaszwagerman">
<img src="https://avatars.githubusercontent.com/u/36264706?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/commits?author=thomaszwagerman">thomaszwagerman</a>
</td>

</tr>

</table>

### Issue Authors

<table>

<tr>

<td align="center">

<a href="https://github.com/helske">
<img src="https://avatars.githubusercontent.com/u/1560448?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+author%3Ahelske">helske</a>
</td>

<td align="center">

<a href="https://github.com/rpodcast">
<img src="https://avatars.githubusercontent.com/u/1043111?u=bb3a363381b39a5172b817ae513c6d10ab1d239a&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+author%3Arpodcast">rpodcast</a>
</td>

<td align="center">

<a href="https://github.com/assignUser">
<img src="https://avatars.githubusercontent.com/u/16141871?u=b8095df6a10813031922a72335bd6579d5494c16&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+author%3AassignUser">assignUser</a>
</td>

<td align="center">

<a href="https://github.com/GFabien">
<img src="https://avatars.githubusercontent.com/u/39873986?u=0c2a28f666efccb0c0b9b23e8584749ab41da789&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+author%3AGFabien">GFabien</a>
</td>

<td align="center">

<a href="https://github.com/pawelru">
<img src="https://avatars.githubusercontent.com/u/12943682?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+author%3Apawelru">pawelru</a>
</td>

<td align="center">

<a href="https://github.com/stitam">
<img src="https://avatars.githubusercontent.com/u/49147718?u=c8736db475d31efad6ebd07b7bc76e4d7241d884&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+author%3Astitam">stitam</a>
</td>

<td align="center">

<a href="https://github.com/willgearty">
<img src="https://avatars.githubusercontent.com/u/7232514?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+author%3Awillgearty">willgearty</a>
</td>

</tr>

</table>

### Issue Contributors

<table>

<tr>

<td align="center">

<a href="https://github.com/krlmlr">
<img src="https://avatars.githubusercontent.com/u/1741643?u=caaf26641c159b84fe1b6d506f57fcea302a556c&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+commenter%3Akrlmlr">krlmlr</a>
</td>

<td align="center">

<a href="https://github.com/noamross">
<img src="https://avatars.githubusercontent.com/u/571752?u=49b086850e1716aa25615cea39250c51e085a5d8&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+commenter%3Anoamross">noamross</a>
</td>

<td align="center">

<a href="https://github.com/maelle">
<img src="https://avatars.githubusercontent.com/u/8360597?u=824f03caa87c92420352e3dd9a05470320a67412&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+commenter%3Amaelle">maelle</a>
</td>

<td align="center">

<a href="https://github.com/mdsumner">
<img src="https://avatars.githubusercontent.com/u/4107631?u=77e928f4bb904a5c2e8927a02194b86662408329&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+commenter%3Amdsumner">mdsumner</a>
</td>

<td align="center">

<a href="https://github.com/kellijohnson-NOAA">
<img src="https://avatars.githubusercontent.com/u/4108564?u=503d9aecc5fadf069c75e493e5abf72c7537b06f&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+commenter%3Akellijohnson-NOAA">kellijohnson-NOAA</a>
</td>

<td align="center">

<a href="https://github.com/ScottClaessens">
<img src="https://avatars.githubusercontent.com/u/26170108?u=af7101b58069fc6dae648825e6aeb1bd4be561e0&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+commenter%3AScottClaessens">ScottClaessens</a>
</td>

<td align="center">

<a href="https://github.com/schneiderpy">
<img src="https://avatars.githubusercontent.com/u/77991319?u=4242d4c5942fced6368dd5c68221e6618092cbf8&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropensci-review-tools/pkgstats/issues?q=is%3Aissue+commenter%3Aschneiderpy">schneiderpy</a>
</td>

</tr>

</table>

<!-- markdownlint-enable -->

<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

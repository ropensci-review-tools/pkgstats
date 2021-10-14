<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/pkgstats/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/pkgstats/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropensci-review-tools/pkgstats/branch/main/graph/badge.svg)](https://codecov.io/gh/ropensci-review-tools/pkgstats)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

# pkgstats

Extract summary statistics of R package structure and functionality.
Also includes a function to extract statistics of all R packages from a
local CRAN mirror. Not all statistics of course, but a good go at
balancing insightful statistics while ensuring computational
feasibility.

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
    from [`ctags`](https::ctags.io), and references (“calls”) to those
    obtained from [`gtags`](https://www.gnu.org/software/global/). This
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
See below for further details.

## Installation

The easiest way to install this package is via the [associated
`r-universe`](https://ropensci-review-tools.r-universe.dev/ui#builds).
As shown there, simply enable the universe with

``` r
options(repos = c(
    ropenscireviewtools = "https://ropensci-review-tools.r-universe.dev",
    CRAN = "https://cloud.r-project.org"))
```

And then install the usual way with,

``` r
install.packages("pkgstats")
```

Alternatively, the package can be installed by running one of the
following lines:

``` r
remotes::install_github ("ropensci-review-tools/pkgstats")
pak::pkg_install ("ropensci-review-tools/pkgstats")
```

The package can then loaded for use with

``` r
library (pkgstats)
```

### Installation on Linux systems

This package requires the system libraries
[`ctags-universal`](https://ctags.io) and [GNU
`global`](https://www.gnu.org/software/global/), both of which are
automatically installed along with the package on both Windows and MacOS
systems. Most Linux distributions do not include a sufficiently
up-to-date version of [`ctags-universal`](https://ctags.io), and so it
must be compiled from source. This can be done by running a single
function, `ctags_install()`, which will install both
[`ctags-universal`](https://ctags.io) and [GNU
`global`](https://www.gnu.org/software/global/).

The `pkgstats` package includes a function to ensure your local
installations of `universal-ctags` and `global` work correctly. Please
ensure you see the following prior to proceeding:

``` r
ctags_test ()
```

    ## ctags installation works as expected

    ## [1] TRUE

Note that GNU `global` can be linked at installation to the Universal
Ctags plug-in parser to expand the [default 5 languages to
30](https://www.gnu.org/software/global/). This makes no difference to
`pkgstats` results, as `gtags` output is only used to trace function
call networks, which is only possible for compiled languages able to
dynamically share pointers to the same objects. This is possible with
the default parser regardless. The wealth of extra information obtained
from linking `global` to the Universal Ctags parser is ultimately
discarded anyway, yet parsing may take considerably longer. If this is
the case, “default” behaviour may be recovered by first running the
following command:

``` r
Sys.unsetenv (c ("GTAGSCONF", "GTAGSLABEL"))
```

See [information on how to install the
plugin](https://cvs.savannah.gnu.org/viewvc/global/global/plugin-factory/PLUGIN_HOWTO.pygments?revision=1.6&view=markup)
for more details.

## Demonstration

The following code demonstrates the output of the main function,
`pkgstats`, applied to the relatively simple [`magrittr`
package](https://github.com/tidyverse/magrittr). The `system.time` call
also shows that these statistics are extracted quite quickly.

``` r
tarball <- "magrittr_2.0.1.tar.gz"
u <- paste0 ("https://cran.r-project.org/src/contrib/",
             tarball)
f <- file.path (tempdir (), tarball)
download.file (u, f)
system.time (
    p <- pkgstats (f)
    )
```

    ##    user  system elapsed 
    ##   0.922   0.141   1.961

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
    ## # A tibble: 3 × 12
    ## # Groups:   language, dir [3]
    ##   language dir   nfiles nlines ncode  ndoc nempty nspaces nchars nexpr ntabs
    ##   <chr>    <chr>  <int>  <int> <int> <int>  <int>   <int>  <int> <dbl> <int>
    ## 1 C        src        2    590   447    22    121    1136  10826     1     0
    ## 2 R        R          7    699   163   484     52    2835  15645     1     1
    ## 3 R        tests     10    374   259    13    102     867   8527     2     4
    ## # … with 1 more variable: indentation <int>
    ## 
    ## $vignettes
    ## vignettes     demos 
    ##         2         0 
    ## 
    ## $data_stats
    ##           n  total_size median_size 
    ##           0           0           0 
    ## 
    ## $desc
    ##    package version                date            license
    ## 1 magrittr   2.0.1 2020-11-17 16:20:06 MIT + file LICENSE
    ##                                                                     urls
    ## 1 https://magrittr.tidyverse.org,\nhttps://github.com/tidyverse/magrittr
    ##                                           bugs aut ctb fnd rev ths trl depends
    ## 1 https://github.com/tidyverse/magrittr/issues   2   0   1   0   0   0      NA
    ##   imports                                suggests linking_to
    ## 1      NA covr, knitr, rlang, rmarkdown, testthat         NA
    ## 
    ## $translations
    ## [1] NA

The first item, `loc`, contains the following Lines-Of-Code and related
statistics, separated into distinct combinations of computer language
and directory:

1.  `nfiles` = Numbers of files in each directory and language.
2.  `nlines` = Total numbers of lines in all files.
3.  `nlines` = Total numbers of lines of code.
4.  `ndoc` = Total numbers of documentation or comment lines.
5.  `nempty` = Total numbers of empty of blank lines.
6.  `nspaces` = Total numbers of white spaces in all code lines,
    excluding leading indentation spaces.
7.  `nchars` = Total numbers of non-white-space characters in all code
    lines.
8.  `nexpr` = Median numbers of nested expressions in all lines which
    have any expressions (see below).
9.  `ntabs` = Number of lines of code with initial tab indentation.
10. `indentation` = Number of spaces by which code is indented (with
    `-1` denoting tab-indentation).

Numbers of nested expressions are counted as numbers of brackets of any
type nested on a single line. The following line has one nested bracket:

``` r
x <- myfn ()
```

while the following has four:

``` r
x <- function () { return (myfn ()) }
```

Code with fewer nested expressions per line is generally easier to read,
and this metric is provided as one indication of the general readability
of code. A second relative indication may be extracted by converting
numbers of spaces and characters to a measure of relative numbers of
white spaces, noting that the `nchars` value quantifies total characters
including white spaces.

``` r
index <- which (p$loc$dir %in% c ("R", "src")) # consider source code only
sum (p$loc$nspaces [index]) / sum (p$loc$nchars [index])
```

    ## [1] 0.1500132

Finally, the `ntabs` statistic can be used to identify whether code uses
tab characters as indentation, otherwise the `indentation` statistics
indicate median numbers of white spaces by which code is indented. The
`objects`, `network`, and `external_calls` items returned by the
[`pkgstats()`
function](https://docs.ropensci.org/pkgstats/reference/pkgstats.html)
are described further below.

### The `pkgstats_summary()` function

A summary of the `pkgstats` data can be obtained by submitting the
object returned from `pkgstats()` to the [`pkgstats_summary()`
function](https://docs.ropensci.org/pkgstats/reference/pkgstats_summary.html):

``` r
s <- pkgstats_summary (p)
```

This function reduces the result of the [`pkgstats()`
function](https://docs.ropensci.org/pkgstats/reference/pkgstats_summary.html)
to a single line with 91 entries, represented as a `data.frame` with one
row and that number of columns. This format is intended to enable
summary statistics from multiple packages to be aggregated by simply
binding rows together. While 91 statistics might seem like overkill, the
[`pkgstats_summary()`
function](https://docs.ropensci.org/pkgstats/reference/pkgstats_summary.html)
aims to return as many usable raw statistics as possible in order to
flexibly allow higher-level statistics to be derived through combination
and aggregation. These 91 statistics can be roughly grouped into the
following categories (not shown in the order in which they actually
appear), with variable names in parentheses after each description.

**Package Summaries**

-   name (`package`)
-   Package version (`version`)
-   Package date, as modification time of `DESCRIPTION` file where not
    explicitly stated (`date`)
-   License (`license`)
-   Languages, as a single comma-separated character value
    (`languages`), and excluding `R` itself.
-   List of translations where package includes translations files,
    given as list of (spoken) language codes (`translations`).

**Information from `DESCRIPTION` file**

-   Package URL(s) (`url`)
-   URL for BugReports (`bugs`)
-   Number of contributors with role of *author* (`desc_n_aut`),
    *contributor* (`desc_n_ctb`), *funder* (`desc_n_fnd`), *reviewer*
    (`desc_n_rev`), *thesis advisor* (`ths`), and *translator* (`trl`,
    relating to translation between computer and not spoken languages).
-   Comma-separated character entries for all `depends`, `imports`,
    `suggests`, and `linking_to` packages.

Numbers of entries in each the of the last two kinds of items can be
obtained from by a simple `strsplit` call, like this:

``` r
length (strsplit (s$suggests, ", ") [[1]])
```

    ## [1] 5

**Numbers of files and associated data**

-   Number of vignettes (`num_vignettes`)
-   Number of demos (`num_demos`)
-   Number of data files (`num_data_files`)
-   Total size of all package data (`data_size_total`)
-   Median size of package data files (`data_size_median`)
-   Numbers of files in main sub-directories (`files_R`, `files_src`,
    `files_inst`, `files_vignettes`, `files_tests`), where numbers are
    recursively counted in all sub-directories, and where `inst` only
    counts files in the `inst/include` sub-directory.

**Statistics on lines of code**

-   Total lines of code in each sub-directory (`loc_R`, `loc_src`,
    `loc_ins`, `loc_vignettes`, `loc_tests`).
-   Total numbers of blank lines in each sub-directory (`blank_lines_R`,
    `blank_lines_src`, `blank_lines_inst`, `blank_lines_vignette`,
    `blank_lines_tests`).
-   Total numbers of comment lines in each sub-directory
    (`comment_lines_R`, `comment_lines_src`, `comment_lines_inst`,
    `comment_lines_vignettes`, `comment_lines_tests`).
-   Measures of relative white space in each sub-directory
    (`rel_space_R`, `rel_space_src`, `rel_space_inst`,
    `rel_space_vignettes`, `rel_space_tests`), as well as an overall
    measure for the `R/`, `src/`, and `inst/` directories (`rel_space`).
-   The number of spaces used to indent code (`indentation`), with
    values of -1 indicating indentation with tab characters.
-   The median number of nested expression per line of code, counting
    only those lines which have any expressions (`nexpr`).

**Statistics on individual objects (including functions)**

These statistics all refer to “functions”, but actually represent more
general “objects,” such as global variables or class definitions
(generally from languages other than R), as detailed below.

-   Numbers of functions in R (`n_fns_r`)
-   Numbers of exported and non-exported R functions
    (`n_fns_r_exported`, `n_fns_r_not_exported`)
-   Number of functions (or objects) in other computer languages
    (`n_fns_src`), including functions in both `src` and `inst/include`
    directories.
-   Number of functions (or objects) per individual file in R and in all
    other (`src`) directories (`n_fns_per_file_r`,
    `n_fns_per_file_src`).
-   Median and mean numbers of parameters per exported R function
    (`npars_exported_mn`, `npars_exported_md`).
-   Mean and median lines of code per function in R and other languages,
    including distinction between exported and non-exported R functions
    (`loc_per_fn_r_mn`, `loc_per_fn_r_md`, `loc_per_fn_r_exp_m`,
    `loc_per_fn_r_exp_md`, `loc_per_fn_r_not_exp_mn`,
    `loc_per_fn_r_not_exp_m`, `loc_per_fn_src_mn`, `loc_per_fn_src_md`).
-   Equivalent mean and median numbers of documentation lines per
    function (`doclines_per_fn_exp_mn`, `doclines_per_fn_exp_md`,
    `doclines_per_fn_not_exp_m`, `doclines_per_fn_not_exp_md`,
    `docchars_per_par_exp_mn`, `docchars_per_par_exp_m`).

**Network Statistics**

The full structure of the `network` table is described below, with
summary statistics including:

-   Number of edges, including distinction between languages (`n_edges`,
    `n_edges_r`, `n_edges_src`).
-   Number of distinct clusters in package network (`n_clusters`).
-   Mean and median centrality of all network edges, calculated from
    both directed and undirected representations of network
    (`centrality_dir_mn`, `centrality_dir_md`, `centrality_undir_mn`,
    `centrality_undir_md`).
-   Equivalent centrality values excluding edges with centrality of zero
    (`centrality_dir_mn_no0`, `centrality_dir_md_no0`,
    `centrality_undir_mn_no0`, `centrality_undir_md_no`).
-   Numbers of terminal edges (`num_terminal_edges_dir`,
    `num_terminal_edges_undir`).
-   Summary statistics on node degree (`node_degree_mn`,
    `node_degree_md`, `node_degree_max`)

**External Call Statistics**

The final column in the result of [the `pkgstats_summary()`
function](https://docs.ropensci.org/pkgstats/reference/pkgstats_summary.html)
summarises the `external_calls` object detailing all calls make to
external packages (including to base and recommended packages). This
summary is represented as a single character string:

``` r
s$external_calls
```

    ## [1] "base:22:12,magrittr:16:11"

This is structured to allow numbers of calls to all packages to be
readily extracted with code like the following:

``` r
calls <- do.call (rbind,
                  strsplit (strsplit (s$external_call, ",") [[1]], ":"))
calls <- data.frame (package = calls [, 1],
                     n_total = as.integer (calls [, 2]),
                     n_unique = as.integer (calls [, 3]))
print (calls)
```

    ##    package n_total n_unique
    ## 1     base      22       12
    ## 2 magrittr      16       11

The two numeric columns respectively show the total number of calls made
to each package, and the total number of unique functions used within
those packages. While this result is relatively uninformative for the
`magrittr` package, which imports no other packages and relies only on
base R functions, these results will generally provide detailed
information on numbers of calls made and functions used.

The following sub-sections provide further detail on the `objects`,
`network`, and `external_call` items, which could be used to extract
additional statistics beyond those described here.

### Objects

The `objects` item contains all code objects identified by the
code-tagging library [`ctags`](https://ctags.io). For R, those are
primarily functions, but for other languages may be a variety of
entities such as class or structure definitions, or sub-members thereof.
Object tables look like this:

``` r
head (p$objects)
```

    ##     file_name     fn_name     kind language loc npars has_dots exported
    ## 1 R/aliases.R     extract function        R   1    NA       NA     TRUE
    ## 2 R/aliases.R    extract2 function        R   1    NA       NA     TRUE
    ## 3 R/aliases.R  use_series function        R   1    NA       NA     TRUE
    ## 4 R/aliases.R         add function        R   1    NA       NA     TRUE
    ## 5 R/aliases.R    subtract function        R   1    NA       NA     TRUE
    ## 6 R/aliases.R multiply_by function        R   1    NA       NA     TRUE
    ##   param_nchars_md param_nchars_mn num_doclines
    ## 1              NA              NA           54
    ## 2              NA              NA           54
    ## 3              NA              NA           54
    ## 4              NA              NA           54
    ## 5              NA              NA           54
    ## 6              NA              NA           54

The `magrittr` package has a total of 191 objects, which the following
lines provide some insight into.

``` r
table (p$objects$language)
```

    ## 
    ##   C   R 
    ##  64 127

``` r
table (p$objects$kind)
```

    ## 
    ##        enum    function functionVar   globalVar        list       macro 
    ##           1          92          27          30           1           3 
    ##      member      struct    variable 
    ##           4           2          31

``` r
table (p$objects$kind [p$objects$language == "R"])
```

    ## 
    ##    function functionVar   globalVar        list 
    ##          69          27          30           1

``` r
table (p$objects$kind [p$objects$language == "C"])
```

    ## 
    ##     enum function    macro   member   struct variable 
    ##        1       23        3        4        2       31

``` r
table (p$objects$kind [p$objects$language == "C++"])
```

    ## < table of extent 0 >

### Network

The `network` item details all relationships between objects, which
generally reflects one object calling or otherwise depending on another
object. Each row thus represents one edge of a “function call” network,
with each entry in the `from` and `to` columns representing the network
vertices or nodes.

``` r
head (p$network)
```

    ##             file line1       from        to language cluster_dir centrality_dir
    ## 1       R/pipe.R   297 new_lambda   freduce        R           1              1
    ## 2    R/getters.R    14  `[[.fseq` functions        R           2              0
    ## 3    R/getters.R    23   `[.fseq` functions        R           2              0
    ## 4  R/functions.R    26 print.fseq functions        R           2              0
    ## 5 R/debug_pipe.R    28 debug_fseq functions        R           2              0
    ## 6 R/debug_pipe.R    35 debug_fseq functions        R           2              0
    ##   cluster_undir centrality_undir
    ## 1             1               17
    ## 2             2                0
    ## 3             2                0
    ## 4             2                0
    ## 5             2                0
    ## 6             2                0

``` r
nrow (p$network)
```

    ## [1] 39

The network table includes additional statistics on the centrality of
each edge, measured as betweenness centrality assuming edges to be both
directed (`centrality_dir`) and undirected (`centrality_undir`). More
central edges reflect connections between objects that are more central
to package functionality, and vice versa. The distinct components of the
network are also represented by discrete cluster numbers, calculated
both for directed and undirected versions of the network. Each distinct
cluster number represents a distinct group of objects, internally
related to other members of the same cluster, yet independent of all
objects with different cluster numbers.

The network can be viewed as an interactive
[`vis.js`](https://visjs.org/) network through passing the result of
`pkgstats` – here, `p` – to the [`plot_network()`
function](https://docs.ropensci.org/pkgstats/reference/plot_network.html).

### External Calls

The `external_calls` item is structured similar to the `network` object,
but identifies all calls to functions from external packages. However,
unlike the `netowrk` and `object` data, which provide information on
objects and relationships in all computer languages used within a
package, the `external_calls` object maps calls within R code only, in
order to provide insight into the use within a package of of functions
from other packages, including R’s base and recommended packages. The
object looks like this:

``` r
head (p$external_calls)
```

    ##   tags_line       call                  tag           file        kind start
    ## 1         1    .onLoad              .onLoad   R/magrittr.R    function    45
    ## 2         7     lapply     `_function_list`       R/pipe.R functionVar   294
    ## 3         7 as_pipe_fn     `_function_list`       R/pipe.R functionVar   294
    ## 4        11        cat anonFunc6fbaaec50100  R/functions.R    function    30
    ## 5        12  invisible anonFuncb07b5cc00100 R/debug_pipe.R    function    35
    ## 6        12      debug anonFuncb07b5cc00100 R/debug_pipe.R    function    35
    ##   end  package
    ## 1  47 magrittr
    ## 2 294     base
    ## 3 294 magrittr
    ## 4  30     base
    ## 5  35     base
    ## 6  35     base

These data are converted to a summary form by the [`pkgstats_summary()`
function](https://docs.ropensci.org/pkgstats/reference/pkgstats_summary.html),
which tabulates numbers of external calls and unique functions from each
package. These data are presented as a single character string which can
be easily converted to the corresponding numeric values using code like
the following:

``` r
x <- strsplit (s$external_calls, ",") [[1]]
x <- do.call (rbind, strsplit (x, ":"))
x <- data.frame (pkg = x [, 1],
                 n_total = as.integer (x [, 2]),
                 n_unique = as.integer (x [, 3]))
x$n_total_rel <- round (x$n_total / sum (x$n_total), 3)
x$n_unique_rel <- round (x$n_unique / sum (x$n_unique), 3)
print (x)
```

    ##        pkg n_total n_unique n_total_rel n_unique_rel
    ## 1     base      22       12       0.579        0.522
    ## 2 magrittr      16       11       0.421        0.478

Those data reveal, for example, that the `magrittr` package makes 22
individual calls to 12 unique functions from the “base” package.

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

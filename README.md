# pkgstats

<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/pkgstats/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/pkgstats/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropensci-review-tools/pkgstats/branch/main/graph/badge.svg)](https://codecov.io/gh/ropensci-review-tools/pkgstats)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Extract summary statistics of R package structure and functionality.
Also includes a function to extract statistics of all R packages from a
local CRAN mirror. Not all statistics of course, but a good go at
balancing insightful statistics while ensuring computational
feasibility.

## What statistics?

Statistics are derived from these primary sources:

1.  Summary of lines-of-code from
    [`cloc`](https://github.com/hrbrmstr/cloc)
2.  Summaries of package `DESCRIPTION` file and a couple of other
    statistics
3.  Summaries of all objects created via package code across multiple
    languages and all directories containing source code (`./R`,
    `./src`, and `./inst/include`).
4.  A function call network derived from function definitions obtained
    from [`ctags`](https::ctags.io), and references (“calls”) to those
    obtained from [`gtags`](https://www.gnu.org/software/global/). This
    network roughly connects every object making a call (as `from`) with
    every object being called (`to`).

A demonstration of typical output is shown below, along with a detailed
list of statistics aggregated by the internal [`pkgstats_summary()`
function](https://ropensci-review-tools.github.io/pkgstats/reference/pkgstats_summary.html).

## So this only performs static code analyses?

Yes. And yes, static code analyses are frequently inadequate, especially
when applied to complex languages like C++. They are nevertheless a darn
sight better than nothing, and they do provide one of the only ways to
construct function call networks across different languages. This
package has been developed with the conviction that the benefits of
being able to extract these networks at all outweigh any disadvantages
arising through potential inaccuracy of static code analyses.

## Installation

This package requires the system libraries
[`ctags-universal`](https://ctags.io) and [GNU
`global`](https://www.gnu.org/software/global/), both of which will be
automatically installed along with the package on both Windows and MacOS
systems, for which running one of the following lines will install
everything:

``` r
remotes::install_github ("ropensci-review-tools/pkgtest")
pak::pkg_install ("ropensci-review-tools/pkgtest")
```

The package can then loaded for use with

``` r
library (pkgstats)
```

### Installation on Linux systems

Most Linux distributions do not include a sufficiently up-to-date
version of [`ctags-universal`](https://ctags.io), and so it must be
compiled from source with the following lines:

``` bash
git clone https://github.com/universal-ctags/ctags.git
cd ctags
./autogen.sh
./configure --prefix=/usr
make
sudo make install
```

[GNU `global`](https://www.gnu.org/software/global/) can generally be
installed from most Linux package managers, for example through
`apt-get install global` for Ubuntu, or `pacman -S global` for
Archlinux. This `pkgstats` package includes a function to ensure your
local installations of `universal-ctags` and `global` work correctly.
Please ensure you see the following prior to proceeding:

``` r
ctags_test ()
```

    ## ctags installation works as expected

    ## [1] TRUE

## Demonstration

The following code demonstrates the output of the main function,
`pkgstats`, applied to the relatively simple [`magrittr`
package](https://github.com/tidyverse/magrittr). The `system.time` call
also shows that these statistics are extracted very quickly.

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
    ##   0.853   0.084   0.928

``` r
names (p)
```

    ## [1] "cloc"          "vignettes"     "data_stats"    "desc"         
    ## [5] "translations"  "code_has_tabs" "objects"       "network"

The result is a list of various data extracted from the code. All except
for `objects` and `network` represent summary data:

``` r
p [!names (p) %in% c ("objects", "network")]
```

    ## $cloc
    ## # A tibble: 5 x 10
    ##   source    language     file_count file_count_pct   loc loc_pct blank_lines
    ##   <chr>     <chr>             <int>          <dbl> <int>   <dbl>       <int>
    ## 1 R         R                     7          0.5     163  0.5             52
    ## 2 src       C                     2          0.333   447  0.461          121
    ## 3 src       C/C++ Header          1          0.167    38  0.0392          12
    ## 4 tests     R                    10          0.5     259  0.5            102
    ## 5 vignettes Rmd                   2          0.5     146  0.5            205
    ## # … with 3 more variables: blank_line_pct <dbl>, comment_lines <int>,
    ## #   comment_line_pct <dbl>
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
    ## 
    ## $code_has_tabs
    ## [1] TRUE

The `objects` and `network` items are described further below.

### The `pkgstats_summary()` function

A summary of the `pkgstats` data can be obtained from the
[`pkgstats_summary()`
function](https://ropensci-review-tools.github.io/pkgstats/reference/pkgstats_summary.html):

``` r
s <- pkgstats_summary (p)
```

This function reduces the result of the [`pkgstats()`
function](https://ropensci-review-tools.github.io/pkgstats/reference/pkgstats.html)
to a single line with 83 entries, represented as a `data.frame` with one
row and that number of columns, enabling multiple packages to be
compared by binding rows together. The following lists describe these 83
statistics (not in the order in which they actually appear), with
variable names in parentheses after each description.

**Package Summaries**

-   name (`package`)
-   Package version (`version`)
-   Package date, as modification time of `DESCRIPTION` file where not
    explicitly stated (`date`)
-   License (`license`)
-   Languages, as a single comma-separated character value
    (`languages`).
-   List of translations where package includes translations files,
    given as list of (spoken) language codes (`languages`).
-   Whether any files within the package include tab characters
    (`code_has_tabs`), which is important because most static code
    parsers are unable to accurately parse code which contains tabs.

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
    recursively counted in all sub-directories.

**Statistics on lines of code**

-   Total lines of code in each sub-directory (`loc_R`, `loc_src`,
    `loc_ins`, `loc_vignettes`, `loc_tests`).
-   Total numbers of blank lines in each sub-directory (`blank_lines_R`,
    `blank_lines_src`, `blank_lines_inst`, `blank_lines_vignette`,
    `blank_lines_tests`).
-   Total numbers of comment lines in each sub-directory
    (`comment_lines_R`, `comment_lines_sr`, `comment_lines_inst`,
    `comment_lines_vignettes`, `comment_lines_tests`).

**Statistics on individual objects (including functions)**

These statistics all refer to “functions”, but actually represent more
general “objects,” such as global variables or class definitions, as
detailed below.

-   Numbers of functions in R (`n_fns_r`)
-   Numbers of exported and non-exported R functions
    (`n_fns_r_exported`, `n_fns_r_not_exported`)
-   Number of functions (or objects) in other computer languages
    (`n_fns_src`), including functions in `src` and `inst/include`
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
    `n_edges_r`, `n_edges_sr`).
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
-   Summary statistics on node degree (`node_degree_m`,
    `node_degree_md`, `node_degree_ma`)

The following sub-sections provide further detail on the `objects` an
`network` items, which could be used to extract additional statistics
beyond those described immediately above.

### Objects

The `objects` item contains all code objects identified by
[`ctags`](https://ctags.io). For R, those are primarily functions, but
for other languages may be a variety of entities such as class or
structure definitions, or sub-members thereof. Object tables look like
this:

``` r
head (p$objects)
```

    ##   file_name     fn_name     kind language loc npars has_dots exported
    ## 1 aliases.R     extract function        R   1    NA       NA     TRUE
    ## 2 aliases.R    extract2 function        R   1    NA       NA     TRUE
    ## 3 aliases.R  use_series function        R   1    NA       NA     TRUE
    ## 4 aliases.R         add function        R   1    NA       NA     TRUE
    ## 5 aliases.R    subtract function        R   1    NA       NA     TRUE
    ## 6 aliases.R multiply_by function        R   1    NA       NA     TRUE
    ##   param_nchars_md param_nchars_mn num_doclines
    ## 1              NA              NA           54
    ## 2              NA              NA           54
    ## 3              NA              NA           54
    ## 4              NA              NA           54
    ## 5              NA              NA           54
    ## 6              NA              NA           54

There are a total of 196 objects, which the following lines provide some
insight into.

``` r
table (p$objects$language)
```

    ## 
    ##   C C++   R 
    ##  64   4 128

``` r
table (p$objects$kind)
```

    ## 
    ##        enum    function functionVar   globalVar        list       macro 
    ##           1          96          27          30           1           4 
    ##      member      struct    variable 
    ##           4           2          31

``` r
table (p$objects$kind [p$objects$language == "R"])
```

    ## 
    ##    function functionVar   globalVar        list 
    ##          70          27          30           1

``` r
table (p$objects$kind [p$objects$language == "C"])
```

    ## 
    ##     enum function    macro   member   struct variable 
    ##        1       23        3        4        2       31

``` r
table (p$objects$kind [p$objects$language == "C++"])
```

    ## 
    ## function    macro 
    ##        3        1

### Network

The `network` item details all relationships between objects, where
generally reflects one object calling or otherwise depending on another
object. Each row thus represents the edge of a network, with each entry
in the `from` and `to` columns representing the network vertices or
nodes.

``` r
head (p$network)
```

    ##             file line1         from        to language cluster_dir
    ## 1       R/pipe.R   297   new_lambda   freduce        R           1
    ## 2    R/getters.R    14    `[[.fseq` functions        R           2
    ## 3    R/getters.R    23     `[.fseq` functions        R           2
    ## 4 R/debug_pipe.R    28   debug_fseq functions        R           2
    ## 5 R/debug_pipe.R    35   debug_fseq functions        R           2
    ## 6 R/debug_pipe.R    42 undebug_fseq functions        R           2
    ##   centrality_dir cluster_undir centrality_undir
    ## 1              1             1               20
    ## 2              0             2                0
    ## 3              0             2                0
    ## 4              0             2                0
    ## 5              0             2                0
    ## 6              0             2                0

``` r
nrow (p$network)
```

    ## [1] 45

The network table includes additional statistics on the centrality of
each edge, measured as betweenness centrality assuming edges to be both
directed (`centrality_dir`) and undirected (`centrality_undir`). More
central edges reflect connections between objects that are more central
to package functionality, and vice versa. The distinct components of the
network are also represented by discrete cluster numbers, object both
for directed and undirected versions of the network. Each distinct
cluster number represents a distinct group of objects, internally
related to other members of the same cluster, yet independent of all
objects with different cluster numbers.

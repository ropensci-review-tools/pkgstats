# pkgstats

<!-- badges: start -->

[![R build
status](https://github.com/mpadge/pkgstats/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/pkgstats/actions?query=workflow%3AR-CMD-check)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Historical statistics of every R package ever. Not all statistics of
course, but a good go at balancing insightful statistics while ensuring
computational feasibility. Requires a local mirror of the full CRAN
archive - that is, all [current
packages](https://cran.r-project.org/web/packages/available_packages_by_name.html),
plus all of their [previous
versions](https://cran.r-project.org/src/contrib/Archive).

## Installation

The package can be installed with:

``` r
remotes::install_github ("mpadge/pkgtest")
```

And then loaded for use with

``` r
library (pkgstats)
```

This package requires [`ctags`](https://ctags.io), and includes a
function to ensure your local installation works correctly. Please
ensure you see the following prior to proceeding:

``` r
ctags_test ()
```

    ## ctags installation works as expected

    ## [1] TRUE

## Demonstration

``` r
library (pkgstats)
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
    ##   0.569   0.067   0.625

``` r
print (p)
```

    ## $cloc
    ## # A tibble: 4 x 10
    ##   source    language     file_count file_count_pct   loc loc_pct blank_lines
    ##   <chr>     <chr>             <int>          <dbl> <int>   <dbl>       <int>
    ## 1 R         R                     7          0.5     163  0.5             52
    ## 2 src       C                     2          0.333   447  0.461          121
    ## 3 src       C/C++ Header          1          0.167    38  0.0392          12
    ## 4 vignettes Rmd                   2          0.5     146  0.5            205
    ## # … with 3 more variables: blank_line_pct <dbl>, comment_lines <int>,
    ## #   comment_line_pct <dbl>
    ## 
    ## $num_vignettes
    ## [1] 3
    ## 
    ## $desc
    ##              license
    ## 1 MIT + file LICENSE
    ##                                                                     urls aut
    ## 1 https://magrittr.tidyverse.org,\nhttps://github.com/tidyverse/magrittr   2
    ##   ctb fnd rev ths trl
    ## 1   0   1   0   0   0
    ## 
    ## $functions
    ##       file_name                fn_name loc npars has_dots exported
    ## 1     aliases.R                extract   1    NA       NA     TRUE
    ## 2     aliases.R               extract2   1    NA       NA     TRUE
    ## 3     aliases.R             use_series   1    NA       NA     TRUE
    ## 4     aliases.R                    add   1    NA       NA     TRUE
    ## 5     aliases.R               subtract   1    NA       NA     TRUE
    ## 6     aliases.R            multiply_by   1    NA       NA     TRUE
    ## 7     aliases.R     multiply_by_matrix   1    NA       NA     TRUE
    ## 8     aliases.R              divide_by   1    NA       NA     TRUE
    ## 9     aliases.R          divide_by_int   1    NA       NA     TRUE
    ## 10    aliases.R         raise_to_power   1    NA       NA     TRUE
    ## 11    aliases.R                    and   1    NA       NA     TRUE
    ## 12    aliases.R                     or   1    NA       NA     TRUE
    ## 13    aliases.R                    mod   1    NA       NA     TRUE
    ## 14    aliases.R                  is_in   1    NA       NA     TRUE
    ## 15    aliases.R                 equals   1    NA       NA     TRUE
    ## 16    aliases.R        is_greater_than   1    NA       NA     TRUE
    ## 17    aliases.R is_weakly_greater_than   1    NA       NA     TRUE
    ## 18    aliases.R           is_less_than   1    NA       NA     TRUE
    ## 19    aliases.R    is_weakly_less_than   1    NA       NA     TRUE
    ## 20    aliases.R                    not   1    NA       NA     TRUE
    ## 21    aliases.R              n'est pas   1    NA       NA     TRUE
    ## 22    aliases.R           set_colnames   1    NA       NA     TRUE
    ## 23    aliases.R           set_rownames   1    NA       NA     TRUE
    ## 24    aliases.R              set_names   1    NA       NA     TRUE
    ## 25    aliases.R              set_class   1    NA       NA     TRUE
    ## 26    aliases.R                  inset   1    NA       NA     TRUE
    ## 27    aliases.R                 inset2   1    NA       NA     TRUE
    ## 28    aliases.R               set_attr   1    NA       NA     TRUE
    ## 29    aliases.R         set_attributes   1    NA       NA     TRUE
    ## 30 debug_pipe.R             debug_pipe   4     1    FALSE     TRUE
    ## 31 debug_pipe.R             debug_fseq   8     2     TRUE     TRUE
    ## 32 debug_pipe.R           undebug_fseq   4     1    FALSE     TRUE
    ## 33    freduce.R                freduce  15     2    FALSE     TRUE
    ## 34  functions.R              functions   5     1    FALSE     TRUE
    ## 35  functions.R             print.fseq   9     2     TRUE     TRUE
    ## 36    getters.R                [[.fseq   3     2     TRUE     TRUE
    ## 37    getters.R                 [.fseq   6     2     TRUE     TRUE
    ## 38   magrittr.R                .onLoad   3     2    FALSE    FALSE
    ## 39       pipe.R                    %>%   8     2    FALSE     TRUE
    ## 40       pipe.R     pipe_eager_lexical   8     2    FALSE     TRUE
    ## 41       pipe.R      pipe_lazy_masking   9     2    FALSE     TRUE
    ## 42       pipe.R            pipe_nested   9     2    FALSE     TRUE
    ## 43       pipe.R                   %<>%   8     2    FALSE     TRUE
    ## 44       pipe.R                   %T>%   8     2    FALSE     TRUE
    ## 45       pipe.R                    %$%   8     2    FALSE     TRUE
    ## 46       pipe.R             new_lambda   5     2    FALSE    FALSE
    ## 47       pipe.R            lambda_fmls   1     2    FALSE    FALSE
    ## 48       pipe.R             as_pipe_fn   3     2    FALSE    FALSE
    ##    param_nchars_md param_nchars_mn num_doclines
    ## 1               NA              NA           54
    ## 2               NA              NA           54
    ## 3               NA              NA           54
    ## 4               NA              NA           54
    ## 5               NA              NA           54
    ## 6               NA              NA           54
    ## 7               NA              NA           54
    ## 8               NA              NA           54
    ## 9               NA              NA           54
    ## 10              NA              NA           54
    ## 11              NA              NA           54
    ## 12              NA              NA           54
    ## 13              NA              NA           54
    ## 14              NA              NA           54
    ## 15              NA              NA           54
    ## 16              NA              NA           54
    ## 17              NA              NA           54
    ## 18              NA              NA           54
    ## 19              NA              NA           54
    ## 20              NA              NA           54
    ## 21              NA              NA           54
    ## 22              NA              NA           54
    ## 23              NA              NA           54
    ## 24              NA              NA           54
    ## 25              NA              NA           54
    ## 26              NA              NA           54
    ## 27              NA              NA           54
    ## 28              NA              NA           54
    ## 29              NA              NA           54
    ## 30             7.0             7.0           11
    ## 31            26.0            26.0           14
    ## 32            26.0            26.0           14
    ## 33            17.0            17.0           12
    ## 34            35.0            35.0           11
    ## 35            18.5            18.5           12
    ## 36            43.5            43.5           19
    ## 37            43.5            43.5           19
    ## 38              NA              NA           NA
    ## 39            40.5            40.5          114
    ## 40            40.5            40.5           13
    ## 41            40.5            40.5           13
    ## 42            40.5            40.5           13
    ## 43            54.0            54.0           42
    ## 44            40.5            40.5           23
    ## 45            43.5            43.5           26
    ## 46              NA              NA           NA
    ## 47              NA              NA           NA
    ## 48              NA              NA           NA

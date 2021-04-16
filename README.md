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

This package requires both [`ctags`](https://ctags.io) and [GNU
`global`](https://www.gnu.org/software/global/), and includes a function
to ensure your local installation of `ctags` works correctly. Please
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
    ##   0.882   0.099   0.966

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
    ## 
    ## $network
    ##              file line1                from                  to     language
    ## 1        R/pipe.R   297          new_lambda             freduce            R
    ## 2     R/getters.R    14           `[[.fseq`           functions            R
    ## 3     R/getters.R    23            `[.fseq`           functions            R
    ## 4   R/functions.R    26          print.fseq           functions            R
    ## 5  R/debug_pipe.R    28          debug_fseq           functions            R
    ## 6  R/debug_pipe.R    35          debug_fseq           functions            R
    ## 7  R/debug_pipe.R    42        undebug_fseq           functions            R
    ## 8  R/debug_pipe.R    43        undebug_fseq           functions            R
    ## 9  R/debug_pipe.R    44        undebug_fseq           functions            R
    ## 10    src/utils.c    77             r_parse         abort_parse   language:C
    ## 11    src/utils.c    80             r_parse         abort_parse   language:C
    ## 13     src/pipe.c   297        as_pipe_call             add_dot   language:C
    ## 15     src/pipe.c   235         pipe_unroll        as_pipe_call   language:C
    ## 16     src/pipe.c   238         pipe_unroll        as_pipe_call   language:C
    ## 17     src/pipe.c   310    as_pipe_tee_call        as_pipe_call   language:C
    ## 19     src/pipe.c   240         pipe_unroll as_pipe_dollar_call   language:C
    ## 21     src/pipe.c   239         pipe_unroll    as_pipe_tee_call   language:C
    ## 23     src/pipe.c   118       magrittr_pipe          clean_pipe   language:C
    ## 24     src/pipe.c   113       magrittr_pipe        cleanup_info   language:C
    ## 25     src/pipe.c   118       magrittr_pipe        cleanup_info   language:C
    ## 26     src/pipe.c   196          clean_pipe        cleanup_info   language:C
    ## 28     src/pipe.c   118       magrittr_pipe           eval_pipe   language:C
    ## 30     src/pipe.c   105       magrittr_pipe      eval_pipe_lazy   language:C
    ## 31     src/pipe.c   324      is_spliced_dot             is_bang   language:C
    ## 32     src/pipe.c   329      is_spliced_dot             is_bang   language:C
    ## 33     src/pipe.c   334      is_spliced_dot             is_bang   language:C
    ## 35     src/pipe.c   177      eval_pipe_lazy           is_return   language:C
    ## 36     src/pipe.c   350             add_dot      is_spliced_dot   language:C
    ## 39     src/pipe.c   415       magrittr_init magrittr_init_utils   language:C
    ## 42     src/pipe.c    88       magrittr_pipe          new_lambda   language:C
    ## 44     src/pipe.c   249         pipe_unroll     parse_pipe_call   language:C
    ## 45     src/pipe.c   109       magrittr_pipe           pipe_info   language:C
    ## 46     src/pipe.c   118       magrittr_pipe           pipe_info   language:C
    ## 47     src/pipe.c   135           eval_pipe           pipe_info   language:C
    ## 49     src/pipe.c    95       magrittr_pipe           pipe_nest   language:C
    ## 51     src/pipe.c    84       magrittr_pipe         pipe_unroll   language:C
    ## 52     src/pipe.c   168      eval_pipe_lazy     r_env_bind_lazy   language:C
    ## 53     src/pipe.c    76       magrittr_pipe           r_env_get language:C++
    ## 54     src/pipe.c    93       magrittr_pipe           r_env_get language:C++
    ## 55     src/pipe.c   101       magrittr_pipe           r_env_get language:C++
    ## 56     src/pipe.c   107       magrittr_pipe           r_env_get language:C++
    ## 57     src/pipe.c   199          clean_pipe        r_env_unbind language:C++
    ## 58     src/pipe.c   162      eval_pipe_lazy   r_new_environment language:C++
    ## 59    src/utils.c    89        r_parse_eval             r_parse   language:C
    ## 60    src/utils.c   123 magrittr_init_utils        r_parse_eval   language:C
    ##    cluster centrality
    ## 1        1         20
    ## 2        2          0
    ## 3        2          0
    ## 4        2          0
    ## 5        2          0
    ## 6        2          0
    ## 7        2          0
    ## 8        2          0
    ## 9        2          0
    ## 10       3          3
    ## 11       3          3
    ## 13       1         54
    ## 15       1        109
    ## 16       1        109
    ## 17       1          0
    ## 19       1        109
    ## 21       1        109
    ## 23       1        171
    ## 24       1        171
    ## 25       1        171
    ## 26       1         20
    ## 28       1        171
    ## 30       1        171
    ## 31       1         20
    ## 32       1         20
    ## 33       1         20
    ## 35       1         57
    ## 36       1         38
    ## 39       3          0
    ## 42       1        171
    ## 44       1        109
    ## 45       1        171
    ## 46       1        171
    ## 47       1          0
    ## 49       1        171
    ## 51       1        171
    ## 52       1         57
    ## 53       1        171
    ## 54       1        171
    ## 55       1        171
    ## 56       1        171
    ## 57       1         20
    ## 58       1         57
    ## 59       3          4
    ## 60       3          3

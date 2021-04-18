# pkgstats

<!-- badges: start -->

[![R build
status](https://github.com/mpadge/pkgstats/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/pkgstats/actions?query=workflow%3AR-CMD-check)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Extract summary statistics of R package structure and functionality.
Also includes a script used to extract historical statistics of every R
package ever. Not all statistics of course, but a good go at balancing
insightful statistics while ensuring computational feasibility. Requires
a local mirror of the full CRAN archive - that is, all [current
packages](https://cran.r-project.org/web/packages/available_packages_by_name.html),
plus all of their [previous
versions](https://cran.r-project.org/src/contrib/Archive).

## What statistics?

Statistics are derived from these primary sources:

1.  Summary of lines-of-code from
    [`cloc`](https://github.com/hrbrmstr/cloc)
2.  Brief summaries of package `DESCRIPTION` file and a couple of other
    statistics
3.  Summaries of all objects created via package code across multiple
    languages and all directories containing source code (`./R`,
    `./src`, and `./inst/include`).
4.  A function call network derived from function definitions obtained
    from [`ctags`](https::ctags.io), and references (“calls”) to those
    obtained from [`gtags`](https://www.gnu.org/software/global/). This
    network roughly connects every object making a call (as `from`) with
    every object being called (`to`).

A demonstration of typical output is shown below. And yes, static code
analyses are frequently inadequate, and especially when applied to
complex languages like C++, but they are nevertheless a darn sight
better than nothing, and they do provide one of the only ways to
construct function call networks across different languages. This
package has been developed with the conviction that the benefits of
being able to extract these networks at all outweigh any disadvantages
arising through potential inaccuracy of static code analyses.

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
    ##   0.753   0.085   0.954

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
    ## $code_has_tabs
    ## [1] TRUE
    ## 
    ## $objects
    ##        file_name                fn_name     kind language loc npars has_dots
    ## 1      aliases.R                extract function        R   1    NA       NA
    ## 2      aliases.R               extract2 function        R   1    NA       NA
    ## 3      aliases.R             use_series function        R   1    NA       NA
    ## 4      aliases.R                    add function        R   1    NA       NA
    ## 5      aliases.R               subtract function        R   1    NA       NA
    ## 6      aliases.R            multiply_by function        R   1    NA       NA
    ## 7      aliases.R     multiply_by_matrix function        R   1    NA       NA
    ## 8      aliases.R              divide_by function        R   1    NA       NA
    ## 9      aliases.R          divide_by_int function        R   1    NA       NA
    ## 10     aliases.R         raise_to_power function        R   1    NA       NA
    ## 11     aliases.R                    and function        R   1    NA       NA
    ## 12     aliases.R                     or function        R   1    NA       NA
    ## 13     aliases.R                    mod function        R   1    NA       NA
    ## 14     aliases.R                  is_in function        R   1    NA       NA
    ## 15     aliases.R                 equals function        R   1    NA       NA
    ## 16     aliases.R        is_greater_than function        R   1    NA       NA
    ## 17     aliases.R is_weakly_greater_than function        R   1    NA       NA
    ## 18     aliases.R           is_less_than function        R   1    NA       NA
    ## 19     aliases.R    is_weakly_less_than function        R   1    NA       NA
    ## 20     aliases.R                    not function        R   1    NA       NA
    ## 21     aliases.R              n'est pas function        R   1    NA       NA
    ## 22     aliases.R           set_colnames function        R   1    NA       NA
    ## 23     aliases.R           set_rownames function        R   1    NA       NA
    ## 24     aliases.R              set_names function        R   1    NA       NA
    ## 25     aliases.R              set_class function        R   1    NA       NA
    ## 26     aliases.R                  inset function        R   1    NA       NA
    ## 27     aliases.R                 inset2 function        R   1    NA       NA
    ## 28     aliases.R               set_attr function        R   1    NA       NA
    ## 29     aliases.R         set_attributes function        R   1    NA       NA
    ## 30  debug_pipe.R             debug_pipe function        R   4     1    FALSE
    ## 31  debug_pipe.R             debug_fseq function        R   8     2     TRUE
    ## 32  debug_pipe.R           undebug_fseq function        R   4     1    FALSE
    ## 33     freduce.R                freduce function        R  15     2    FALSE
    ## 34   functions.R              functions function        R   5     1    FALSE
    ## 35   functions.R             print.fseq function        R   9     2     TRUE
    ## 36     getters.R                [[.fseq function        R   3     2     TRUE
    ## 37     getters.R                 [.fseq function        R   6     2     TRUE
    ## 38    magrittr.R                .onLoad function        R   3     2    FALSE
    ## 39        pipe.R                    %>% function        R   8     2    FALSE
    ## 40        pipe.R     pipe_eager_lexical function        R   8     2    FALSE
    ## 41        pipe.R      pipe_lazy_masking function        R   9     2    FALSE
    ## 42        pipe.R            pipe_nested function        R   9     2    FALSE
    ## 43        pipe.R                   %<>% function        R   8     2    FALSE
    ## 44        pipe.R                   %T>% function        R   8     2    FALSE
    ## 45        pipe.R                    %$% function        R   8     2    FALSE
    ## 46        pipe.R             new_lambda function        R   5     2    FALSE
    ## 47        pipe.R            lambda_fmls function        R   1     2    FALSE
    ## 48        pipe.R             as_pipe_fn function        R   3     2    FALSE
    ## 110  src/utils.h       MAGRITTR_UTILS_H    macro      C++   1    NA       NA
    ## 74    src/pipe.c             R_NO_REMAP    macro        C   1    NA       NA
    ## 81   src/utils.c             R_NO_REMAP    macro        C   1    NA       NA
    ## 91    src/pipe.c        R_init_magrittr function        C   4    NA       NA
    ## 101  src/utils.c            abort_parse function        C   6    NA       NA
    ## 111   src/pipe.c                add_dot function        C  16    NA       NA
    ## 121   src/pipe.c           as_pipe_call function        C  13    NA       NA
    ## 131   src/pipe.c    as_pipe_dollar_call function        C   3    NA       NA
    ## 141   src/pipe.c       as_pipe_tee_call function        C   7    NA       NA
    ## 151   src/pipe.c           call_entries variable        C   4    NA       NA
    ## 161   src/pipe.c        calls_base_with variable        C   1    NA       NA
    ## 171   src/pipe.c               chrs_dot variable        C   1    NA       NA
    ## 181   src/pipe.c             clean_pipe function        C   9    NA       NA
    ## 191   src/pipe.c           cleanup_info   struct        C   4    NA       NA
    ## 201   src/pipe.c                    env   member        C   1    NA       NA
    ## 211   src/pipe.c                    env   member        C   1    NA       NA
    ## 221   src/pipe.c              eval_pipe function        C  18    NA       NA
    ## 231   src/pipe.c         eval_pipe_lazy function        C  34    NA       NA
    ## 241   src/pipe.c                 export    macro        C   1    NA       NA
    ## 251   src/pipe.c                  exprs   member        C   1    NA       NA
    ## 261   src/pipe.c            ext_entries variable        C   4    NA       NA
    ## 271   src/pipe.c                is_bang function        C   3    NA       NA
    ## 281   src/pipe.c              is_return function        C   3    NA       NA
    ## 291   src/pipe.c         is_spliced_dot function        C  17    NA       NA
    ## 301   src/pipe.c          magrittr_init function        C  36    NA       NA
    ## 311  src/utils.c    magrittr_init_utils function        C  12    NA       NA
    ## 321   src/pipe.c        magrittr_ns_env variable        C   1    NA       NA
    ## 331   src/pipe.c          magrittr_pipe function        C  64    NA       NA
    ## 341  src/utils.c   new_env__parent_node variable        C   1    NA       NA
    ## 351  src/utils.c     new_env__size_node variable        C   1    NA       NA
    ## 361  src/utils.c           new_env_call variable        C   1    NA       NA
    ## 371   src/pipe.c             new_lambda function        C   7    NA       NA
    ## 381   src/pipe.c                    old   member        C   1    NA       NA
    ## 391   src/pipe.c        parse_pipe_call function        C  22    NA       NA
    ## 401   src/pipe.c              pipe_info   struct        C   4    NA       NA
    ## 411   src/pipe.c              pipe_kind     enum        C   7    NA       NA
    ## 421   src/pipe.c              pipe_nest function        C  37    NA       NA
    ## 431   src/pipe.c            pipe_unroll function        C  54    NA       NA
    ## 441  src/utils.c          r__env_unbind function        C  23    NA       NA
    ## 451  src/utils.c        r_env_bind_lazy function        C  18    NA       NA
    ## 461  src/utils.h              r_env_get function      C++  11    NA       NA
    ## 471  src/utils.h           r_env_unbind function      C++   8    NA       NA
    ## 481  src/utils.h      r_new_environment function      C++   5    NA       NA
    ## 49   src/utils.c                r_parse function        C  17    NA       NA
    ## 50   src/utils.c           r_parse_eval function        C   5    NA       NA
    ## 51    src/pipe.c            syms_assign variable        C   1    NA       NA
    ## 52    src/pipe.c              syms_bang variable        C   1    NA       NA
    ## 53    src/pipe.c             syms_curly variable        C   1    NA       NA
    ## 54   src/utils.c    syms_delayed_assign variable        C   1    NA       NA
    ## 55    src/pipe.c               syms_dot variable        C   1    NA       NA
    ## 56    src/pipe.c               syms_env variable        C   1    NA       NA
    ## 57   src/utils.c             syms_envir variable        C   1    NA       NA
    ## 58   src/utils.c          syms_inherits variable        C   1    NA       NA
    ## 59    src/pipe.c              syms_kind variable        C   1    NA       NA
    ## 60    src/pipe.c              syms_lazy variable        C   1    NA       NA
    ## 61    src/pipe.c               syms_lhs variable        C   1    NA       NA
    ## 62   src/utils.c              syms_list variable        C   1    NA       NA
    ## 63    src/pipe.c            syms_nested variable        C   1    NA       NA
    ## 64    src/pipe.c        syms_new_lambda variable        C   1    NA       NA
    ## 65    src/pipe.c             syms_paren variable        C   1    NA       NA
    ## 66    src/pipe.c              syms_pipe variable        C   1    NA       NA
    ## 67    src/pipe.c     syms_pipe_compound variable        C   1    NA       NA
    ## 68    src/pipe.c       syms_pipe_dollar variable        C   1    NA       NA
    ## 69    src/pipe.c          syms_pipe_tee variable        C   1    NA       NA
    ## 70    src/pipe.c            syms_return variable        C   1    NA       NA
    ## 71    src/pipe.c               syms_rhs variable        C   1    NA       NA
    ## 72   src/utils.c                syms_rm variable        C   1    NA       NA
    ## 73    src/pipe.c               syms_sym variable        C   1    NA       NA
    ##     exported param_nchars_md param_nchars_mn num_doclines
    ## 1       TRUE              NA              NA           54
    ## 2       TRUE              NA              NA           54
    ## 3       TRUE              NA              NA           54
    ## 4       TRUE              NA              NA           54
    ## 5       TRUE              NA              NA           54
    ## 6       TRUE              NA              NA           54
    ## 7       TRUE              NA              NA           54
    ## 8       TRUE              NA              NA           54
    ## 9       TRUE              NA              NA           54
    ## 10      TRUE              NA              NA           54
    ## 11      TRUE              NA              NA           54
    ## 12      TRUE              NA              NA           54
    ## 13      TRUE              NA              NA           54
    ## 14      TRUE              NA              NA           54
    ## 15      TRUE              NA              NA           54
    ## 16      TRUE              NA              NA           54
    ## 17      TRUE              NA              NA           54
    ## 18      TRUE              NA              NA           54
    ## 19      TRUE              NA              NA           54
    ## 20      TRUE              NA              NA           54
    ## 21      TRUE              NA              NA           54
    ## 22      TRUE              NA              NA           54
    ## 23      TRUE              NA              NA           54
    ## 24      TRUE              NA              NA           54
    ## 25      TRUE              NA              NA           54
    ## 26      TRUE              NA              NA           54
    ## 27      TRUE              NA              NA           54
    ## 28      TRUE              NA              NA           54
    ## 29      TRUE              NA              NA           54
    ## 30      TRUE             7.0             7.0           11
    ## 31      TRUE            26.0            26.0           14
    ## 32      TRUE            26.0            26.0           14
    ## 33      TRUE            17.0            17.0           12
    ## 34      TRUE            35.0            35.0           11
    ## 35      TRUE            18.5            18.5           12
    ## 36      TRUE            43.5            43.5           19
    ## 37      TRUE            43.5            43.5           19
    ## 38     FALSE              NA              NA           NA
    ## 39      TRUE            40.5            40.5          114
    ## 40      TRUE            40.5            40.5           13
    ## 41      TRUE            40.5            40.5           13
    ## 42      TRUE            40.5            40.5           13
    ## 43      TRUE            54.0            54.0           42
    ## 44      TRUE            40.5            40.5           23
    ## 45      TRUE            43.5            43.5           26
    ## 46     FALSE              NA              NA           NA
    ## 47     FALSE              NA              NA           NA
    ## 48     FALSE              NA              NA           NA
    ## 110    FALSE              NA              NA           NA
    ## 74     FALSE              NA              NA           NA
    ## 81     FALSE              NA              NA           NA
    ## 91     FALSE              NA              NA           NA
    ## 101    FALSE              NA              NA           NA
    ## 111    FALSE              NA              NA           NA
    ## 121    FALSE              NA              NA           NA
    ## 131    FALSE              NA              NA           NA
    ## 141    FALSE              NA              NA           NA
    ## 151    FALSE              NA              NA           NA
    ## 161    FALSE              NA              NA           NA
    ## 171    FALSE              NA              NA           NA
    ## 181    FALSE              NA              NA           NA
    ## 191    FALSE              NA              NA           NA
    ## 201    FALSE              NA              NA           NA
    ## 211    FALSE              NA              NA           NA
    ## 221    FALSE              NA              NA           NA
    ## 231    FALSE              NA              NA           NA
    ## 241    FALSE              NA              NA           NA
    ## 251    FALSE              NA              NA           NA
    ## 261    FALSE              NA              NA           NA
    ## 271    FALSE              NA              NA           NA
    ## 281    FALSE              NA              NA           NA
    ## 291    FALSE              NA              NA           NA
    ## 301    FALSE              NA              NA           NA
    ## 311    FALSE              NA              NA           NA
    ## 321    FALSE              NA              NA           NA
    ## 331    FALSE              NA              NA           NA
    ## 341    FALSE              NA              NA           NA
    ## 351    FALSE              NA              NA           NA
    ## 361    FALSE              NA              NA           NA
    ## 371    FALSE              NA              NA           NA
    ## 381    FALSE              NA              NA           NA
    ## 391    FALSE              NA              NA           NA
    ## 401    FALSE              NA              NA           NA
    ## 411    FALSE              NA              NA           NA
    ## 421    FALSE              NA              NA           NA
    ## 431    FALSE              NA              NA           NA
    ## 441    FALSE              NA              NA           NA
    ## 451    FALSE              NA              NA           NA
    ## 461    FALSE              NA              NA           NA
    ## 471    FALSE              NA              NA           NA
    ## 481    FALSE              NA              NA           NA
    ## 49     FALSE              NA              NA           NA
    ## 50     FALSE              NA              NA           NA
    ## 51     FALSE              NA              NA           NA
    ## 52     FALSE              NA              NA           NA
    ## 53     FALSE              NA              NA           NA
    ## 54     FALSE              NA              NA           NA
    ## 55     FALSE              NA              NA           NA
    ## 56     FALSE              NA              NA           NA
    ## 57     FALSE              NA              NA           NA
    ## 58     FALSE              NA              NA           NA
    ## 59     FALSE              NA              NA           NA
    ## 60     FALSE              NA              NA           NA
    ## 61     FALSE              NA              NA           NA
    ## 62     FALSE              NA              NA           NA
    ## 63     FALSE              NA              NA           NA
    ## 64     FALSE              NA              NA           NA
    ## 65     FALSE              NA              NA           NA
    ## 66     FALSE              NA              NA           NA
    ## 67     FALSE              NA              NA           NA
    ## 68     FALSE              NA              NA           NA
    ## 69     FALSE              NA              NA           NA
    ## 70     FALSE              NA              NA           NA
    ## 71     FALSE              NA              NA           NA
    ## 72     FALSE              NA              NA           NA
    ## 73     FALSE              NA              NA           NA
    ## 
    ## $network
    ##              file line1                from                  to     language
    ## 1        R/pipe.R   297          new_lambda             freduce            R
    ## 2     R/getters.R    14           `[[.fseq`           functions            R
    ## 3     R/getters.R    23            `[.fseq`           functions            R
    ## 4  R/debug_pipe.R    28          debug_fseq           functions            R
    ## 5  R/debug_pipe.R    35          debug_fseq           functions            R
    ## 6  R/debug_pipe.R    42        undebug_fseq           functions            R
    ## 7  R/debug_pipe.R    43        undebug_fseq           functions            R
    ## 8  R/debug_pipe.R    44        undebug_fseq           functions            R
    ## 9   R/functions.R    26          print.fseq           functions            R
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
    ##    cluster_dir centrality_dir cluster_undir centrality_undir
    ## 1            1              1             1               20
    ## 2            2              0             2                0
    ## 3            2              0             2                0
    ## 4            2              0             2                0
    ## 5            2              0             2                0
    ## 6            2              0             2                0
    ## 7            2              0             2                0
    ## 8            2              0             2                0
    ## 9            2              0             2                0
    ## 10           3              3             3                3
    ## 11           3              3             3                3
    ## 13           1              9             1               54
    ## 15           1              7             1              109
    ## 16           1              7             1              109
    ## 17           1              0             1                0
    ## 19           1              7             1              109
    ## 21           1              7             1              109
    ## 23           1              0             1              171
    ## 24           1              0             1              171
    ## 25           1              0             1              171
    ## 26           1              1             1               20
    ## 28           1              0             1              171
    ## 30           1              0             1              171
    ## 31           1              5             1               20
    ## 32           1              5             1               20
    ## 33           1              5             1               20
    ## 35           1              3             1               57
    ## 36           1              8             1               38
    ## 39           3              0             3                0
    ## 42           1              0             1              171
    ## 44           1              7             1              109
    ## 45           1              0             1              171
    ## 46           1              0             1              171
    ## 47           1              0             1                0
    ## 49           1              0             1              171
    ## 51           1              0             1              171
    ## 52           1              3             1               57
    ## 53           1              0             1              171
    ## 54           1              0             1              171
    ## 55           1              0             1              171
    ## 56           1              0             1              171
    ## 57           1              1             1               20
    ## 58           1              3             1               57
    ## 59           3              4             3                4
    ## 60           3              3             3                3

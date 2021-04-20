# pkgstats

<!-- badges: start -->

[![R build
status](https://github.com/mpadge/pkgstats/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/pkgstats/actions?query=workflow%3AR-CMD-check)
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
    ##   0.688   0.059   0.736

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
    ## $vignettes
    ## vignettes     demos 
    ##         2         0 
    ## 
    ## $data_stats
    ##           n  total_size median_size 
    ##           0           0           0 
    ## 
    ## $desc
    ##    package version                    date            license
    ## 1 magrittr   2.0.1 2020-11-17 16:20:06 UTC MIT + file LICENSE
    ##                                                                     urls
    ## 1 https://magrittr.tidyverse.org,\nhttps://github.com/tidyverse/magrittr
    ##                                           bugs aut ctb fnd rev ths trl depends
    ## 1 https://github.com/tidyverse/magrittr/issues   2   0   1   0   0   0      NA
    ##   imports                                suggests linking_to
    ## 1      NA covr, knitr, rlang, rmarkdown, testthat         NA
    ## 
    ## $code_has_tabs
    ## [1] TRUE
    ## 
    ## $objects
    ##           file_name                fn_name        kind language loc npars
    ## 1         aliases.R                extract    function        R   1    NA
    ## 2         aliases.R               extract2    function        R   1    NA
    ## 3         aliases.R             use_series    function        R   1    NA
    ## 4         aliases.R                    add    function        R   1    NA
    ## 5         aliases.R               subtract    function        R   1    NA
    ## 6         aliases.R            multiply_by    function        R   1    NA
    ## 7         aliases.R     multiply_by_matrix    function        R   1    NA
    ## 8         aliases.R              divide_by    function        R   1    NA
    ## 9         aliases.R          divide_by_int    function        R   1    NA
    ## 10        aliases.R         raise_to_power    function        R   1    NA
    ## 11        aliases.R                    and    function        R   1    NA
    ## 12        aliases.R                     or    function        R   1    NA
    ## 13        aliases.R                    mod    function        R   1    NA
    ## 14        aliases.R                  is_in    function        R   1    NA
    ## 15        aliases.R                 equals    function        R   1    NA
    ## 16        aliases.R        is_greater_than    function        R   1    NA
    ## 17        aliases.R is_weakly_greater_than    function        R   1    NA
    ## 18        aliases.R           is_less_than    function        R   1    NA
    ## 19        aliases.R    is_weakly_less_than    function        R   1    NA
    ## 20        aliases.R                    not    function        R   1    NA
    ## 21        aliases.R              n'est pas    function        R   1    NA
    ## 22        aliases.R           set_colnames    function        R   1    NA
    ## 23        aliases.R           set_rownames    function        R   1    NA
    ## 24        aliases.R              set_names    function        R   1    NA
    ## 25        aliases.R              set_class    function        R   1    NA
    ## 26        aliases.R                  inset    function        R   1    NA
    ## 27        aliases.R                 inset2    function        R   1    NA
    ## 28        aliases.R               set_attr    function        R   1    NA
    ## 29        aliases.R         set_attributes    function        R   1    NA
    ## 30     debug_pipe.R             debug_pipe    function        R   4     1
    ## 31     debug_pipe.R             debug_fseq    function        R   8     2
    ## 32     debug_pipe.R           undebug_fseq    function        R   4     1
    ## 33        freduce.R                freduce    function        R  15     2
    ## 34      functions.R              functions    function        R   5     1
    ## 35      functions.R             print.fseq    function        R   9     2
    ## 36        getters.R                [[.fseq    function        R   3     2
    ## 37        getters.R                 [.fseq    function        R   6     2
    ## 38       magrittr.R                .onLoad    function        R   3     2
    ## 39           pipe.R                    %>%    function        R   8     2
    ## 40           pipe.R     pipe_eager_lexical    function        R   8     2
    ## 41           pipe.R      pipe_lazy_masking    function        R   9     2
    ## 42           pipe.R            pipe_nested    function        R   9     2
    ## 43           pipe.R                   %<>%    function        R   8     2
    ## 44           pipe.R                   %T>%    function        R   8     2
    ## 45           pipe.R                    %$%    function        R   8     2
    ## 46           pipe.R             new_lambda    function        R   5     2
    ## 47           pipe.R            lambda_fmls    function        R   1     2
    ## 48           pipe.R             as_pipe_fn    function        R   3     2
    ## 156    R/magrittr.R                .onLoad    function        R   3    NA
    ## 210        R/pipe.R                  `%$%`    function        R   8    NA
    ## 310        R/pipe.R                 `%<>%`    function        R   8    NA
    ## 410        R/pipe.R                  `%>%`    function        R   8    NA
    ## 510        R/pipe.R                 `%T>%`    function        R   8    NA
    ## 610     R/getters.R               `[.fseq`    function        R   7    NA
    ## 710     R/getters.R              `[[.fseq`    function        R   4    NA
    ## 84         R/pipe.R       `_function_list` functionVar        R   1    NA
    ## 910     R/aliases.R            `n'est pas`   globalVar        R   1    NA
    ## 1010    R/aliases.R                    add   globalVar        R   1    NA
    ## 1110    R/aliases.R                    and   globalVar        R   1    NA
    ## 1210  R/functions.R   anonFunc61e3f7090100    function        R   1    NA
    ## 1310       R/pipe.R   anonFunce61bbebe0100    function        R   1    NA
    ## 1410 R/debug_pipe.R   anonFunce7cdad840100    function        R   1    NA
    ## 157        R/pipe.R             as_pipe_fn    function        R   3    NA
    ## 161  R/debug_pipe.R             debug_fseq    function        R  11    NA
    ## 171  R/debug_pipe.R             debug_pipe    function        R   5    NA
    ## 181     R/aliases.R              divide_by   globalVar        R   1    NA
    ## 191     R/aliases.R          divide_by_int   globalVar        R   1    NA
    ## 201        R/pipe.R                    env functionVar        R   1    NA
    ## 211        R/pipe.R                    env functionVar        R   1    NA
    ## 221        R/pipe.R                    env functionVar        R   1    NA
    ## 231        R/pipe.R                    env functionVar        R   1    NA
    ## 241        R/pipe.R                    env functionVar        R   1    NA
    ## 251        R/pipe.R                    env functionVar        R   1    NA
    ## 261        R/pipe.R                    env functionVar        R   1    NA
    ## 271     R/aliases.R                 equals   globalVar        R   1    NA
    ## 281     R/aliases.R                extract   globalVar        R   1    NA
    ## 291     R/aliases.R               extract2   globalVar        R   1    NA
    ## 301   R/functions.R                  flist functionVar        R   1    NA
    ## 311     R/freduce.R                freduce    function        R  16    NA
    ## 321   R/functions.R              functions    function        R   6    NA
    ## 351  R/debug_pipe.R                indices        list        R   1    NA
    ## 361     R/aliases.R                  inset   globalVar        R   1    NA
    ## 371     R/aliases.R                 inset2   globalVar        R   1    NA
    ## 381     R/aliases.R        is_greater_than   globalVar        R   1    NA
    ## 391     R/aliases.R                  is_in   globalVar        R   1    NA
    ## 401     R/aliases.R           is_less_than   globalVar        R   1    NA
    ## 411  R/debug_pipe.R         is_valid_index    function        R   1    NA
    ## 421     R/aliases.R is_weakly_greater_than   globalVar        R   1    NA
    ## 431     R/aliases.R    is_weakly_less_than   globalVar        R   1    NA
    ## 441     R/freduce.R                      k functionVar        R   1    NA
    ## 451        R/pipe.R                   kind functionVar        R   1    NA
    ## 461        R/pipe.R                   kind functionVar        R   1    NA
    ## 471        R/pipe.R                   kind functionVar        R   1    NA
    ## 481        R/pipe.R                   kind functionVar        R   1    NA
    ## 49         R/pipe.R                   kind functionVar        R   1    NA
    ## 50         R/pipe.R                   kind functionVar        R   1    NA
    ## 51         R/pipe.R                   kind functionVar        R   1    NA
    ## 52         R/pipe.R            lambda_fmls   globalVar        R   1    NA
    ## 53         R/pipe.R                   lazy functionVar        R   1    NA
    ## 54         R/pipe.R                   lazy functionVar        R   1    NA
    ## 55         R/pipe.R                   lazy functionVar        R   1    NA
    ## 56         R/pipe.R                   lazy functionVar        R   1    NA
    ## 57         R/pipe.R                   lazy functionVar        R   1    NA
    ## 58      R/aliases.R                    mod   globalVar        R   1    NA
    ## 59      R/aliases.R            multiply_by   globalVar        R   1    NA
    ## 60      R/aliases.R     multiply_by_matrix   globalVar        R   1    NA
    ## 61         R/pipe.R                 nested functionVar        R   1    NA
    ## 62         R/pipe.R             new_lambda    function        R   8    NA
    ## 63      R/aliases.R                    not   globalVar        R   1    NA
    ## 64      R/aliases.R                     or   globalVar        R   1    NA
    ## 65         R/pipe.R     pipe_eager_lexical    function        R   8    NA
    ## 66         R/pipe.R      pipe_lazy_masking    function        R   9    NA
    ## 67         R/pipe.R            pipe_nested    function        R   9    NA
    ## 68    R/functions.R             print.fseq    function        R  10    NA
    ## 69      R/aliases.R         raise_to_power   globalVar        R   1    NA
    ## 70      R/aliases.R               set_attr   globalVar        R   1    NA
    ## 71      R/aliases.R         set_attributes   globalVar        R   1    NA
    ## 72      R/aliases.R              set_class   globalVar        R   1    NA
    ## 73      R/aliases.R           set_colnames   globalVar        R   1    NA
    ## 74      R/aliases.R              set_names   globalVar        R   1    NA
    ## 75      R/aliases.R           set_rownames   globalVar        R   1    NA
    ## 76      R/aliases.R               subtract   globalVar        R   1    NA
    ## 77         R/pipe.R                    sym functionVar        R   1    NA
    ## 78         R/pipe.R                    sym functionVar        R   1    NA
    ## 79         R/pipe.R                    sym functionVar        R   1    NA
    ## 80   R/debug_pipe.R           undebug_fseq    function        R   6    NA
    ## 81      R/aliases.R             use_series   globalVar        R   1    NA
    ## 82      R/getters.R                      y functionVar        R   1    NA
    ## 83      src/utils.h       MAGRITTR_UTILS_H       macro      C++   1    NA
    ## 89       src/pipe.c             R_NO_REMAP       macro        C   1    NA
    ## 90      src/utils.c             R_NO_REMAP       macro        C   1    NA
    ## 91       src/pipe.c        R_init_magrittr    function        C   4    NA
    ## 92      src/utils.c            abort_parse    function        C   6    NA
    ## 93       src/pipe.c                add_dot    function        C  16    NA
    ## 94       src/pipe.c           as_pipe_call    function        C  13    NA
    ## 95       src/pipe.c    as_pipe_dollar_call    function        C   3    NA
    ## 96       src/pipe.c       as_pipe_tee_call    function        C   7    NA
    ## 97       src/pipe.c           call_entries    variable        C   4    NA
    ## 98       src/pipe.c        calls_base_with    variable        C   1    NA
    ## 99       src/pipe.c               chrs_dot    variable        C   1    NA
    ## 100      src/pipe.c             clean_pipe    function        C   9    NA
    ## 101      src/pipe.c           cleanup_info      struct        C   4    NA
    ## 102      src/pipe.c                    env      member        C   1    NA
    ## 103      src/pipe.c                    env      member        C   1    NA
    ## 104      src/pipe.c              eval_pipe    function        C  18    NA
    ## 105      src/pipe.c         eval_pipe_lazy    function        C  34    NA
    ## 106      src/pipe.c                 export       macro        C   1    NA
    ## 107      src/pipe.c                  exprs      member        C   1    NA
    ## 108      src/pipe.c            ext_entries    variable        C   4    NA
    ## 109      src/pipe.c                is_bang    function        C   3    NA
    ## 110      src/pipe.c              is_return    function        C   3    NA
    ## 111      src/pipe.c         is_spliced_dot    function        C  17    NA
    ## 112      src/pipe.c          magrittr_init    function        C  36    NA
    ## 113     src/utils.c    magrittr_init_utils    function        C  12    NA
    ## 114      src/pipe.c        magrittr_ns_env    variable        C   1    NA
    ## 115      src/pipe.c          magrittr_pipe    function        C  64    NA
    ## 116     src/utils.c   new_env__parent_node    variable        C   1    NA
    ## 117     src/utils.c     new_env__size_node    variable        C   1    NA
    ## 118     src/utils.c           new_env_call    variable        C   1    NA
    ## 119      src/pipe.c             new_lambda    function        C   7    NA
    ## 120      src/pipe.c                    old      member        C   1    NA
    ## 121      src/pipe.c        parse_pipe_call    function        C  22    NA
    ## 122      src/pipe.c              pipe_info      struct        C   4    NA
    ## 123      src/pipe.c              pipe_kind        enum        C   7    NA
    ## 124      src/pipe.c              pipe_nest    function        C  37    NA
    ## 125      src/pipe.c            pipe_unroll    function        C  54    NA
    ## 126     src/utils.c          r__env_unbind    function        C  23    NA
    ## 127     src/utils.c        r_env_bind_lazy    function        C  18    NA
    ## 128     src/utils.h              r_env_get    function      C++  11    NA
    ## 129     src/utils.h           r_env_unbind    function      C++   8    NA
    ## 130     src/utils.h      r_new_environment    function      C++   5    NA
    ## 131     src/utils.c                r_parse    function        C  17    NA
    ## 132     src/utils.c           r_parse_eval    function        C   5    NA
    ## 133      src/pipe.c            syms_assign    variable        C   1    NA
    ## 134      src/pipe.c              syms_bang    variable        C   1    NA
    ## 135      src/pipe.c             syms_curly    variable        C   1    NA
    ## 136     src/utils.c    syms_delayed_assign    variable        C   1    NA
    ## 137      src/pipe.c               syms_dot    variable        C   1    NA
    ## 138      src/pipe.c               syms_env    variable        C   1    NA
    ## 139     src/utils.c             syms_envir    variable        C   1    NA
    ## 140     src/utils.c          syms_inherits    variable        C   1    NA
    ## 141      src/pipe.c              syms_kind    variable        C   1    NA
    ## 142      src/pipe.c              syms_lazy    variable        C   1    NA
    ## 143      src/pipe.c               syms_lhs    variable        C   1    NA
    ## 144     src/utils.c              syms_list    variable        C   1    NA
    ## 145      src/pipe.c            syms_nested    variable        C   1    NA
    ## 146      src/pipe.c        syms_new_lambda    variable        C   1    NA
    ## 147      src/pipe.c             syms_paren    variable        C   1    NA
    ## 148      src/pipe.c              syms_pipe    variable        C   1    NA
    ## 149      src/pipe.c     syms_pipe_compound    variable        C   1    NA
    ## 150      src/pipe.c       syms_pipe_dollar    variable        C   1    NA
    ## 151      src/pipe.c          syms_pipe_tee    variable        C   1    NA
    ## 152      src/pipe.c            syms_return    variable        C   1    NA
    ## 153      src/pipe.c               syms_rhs    variable        C   1    NA
    ## 154     src/utils.c                syms_rm    variable        C   1    NA
    ## 155      src/pipe.c               syms_sym    variable        C   1    NA
    ##      has_dots exported param_nchars_md param_nchars_mn num_doclines
    ## 1          NA     TRUE              NA              NA           54
    ## 2          NA     TRUE              NA              NA           54
    ## 3          NA     TRUE              NA              NA           54
    ## 4          NA     TRUE              NA              NA           54
    ## 5          NA     TRUE              NA              NA           54
    ## 6          NA     TRUE              NA              NA           54
    ## 7          NA     TRUE              NA              NA           54
    ## 8          NA     TRUE              NA              NA           54
    ## 9          NA     TRUE              NA              NA           54
    ## 10         NA     TRUE              NA              NA           54
    ## 11         NA     TRUE              NA              NA           54
    ## 12         NA     TRUE              NA              NA           54
    ## 13         NA     TRUE              NA              NA           54
    ## 14         NA     TRUE              NA              NA           54
    ## 15         NA     TRUE              NA              NA           54
    ## 16         NA     TRUE              NA              NA           54
    ## 17         NA     TRUE              NA              NA           54
    ## 18         NA     TRUE              NA              NA           54
    ## 19         NA     TRUE              NA              NA           54
    ## 20         NA     TRUE              NA              NA           54
    ## 21         NA     TRUE              NA              NA           54
    ## 22         NA     TRUE              NA              NA           54
    ## 23         NA     TRUE              NA              NA           54
    ## 24         NA     TRUE              NA              NA           54
    ## 25         NA     TRUE              NA              NA           54
    ## 26         NA     TRUE              NA              NA           54
    ## 27         NA     TRUE              NA              NA           54
    ## 28         NA     TRUE              NA              NA           54
    ## 29         NA     TRUE              NA              NA           54
    ## 30      FALSE     TRUE             7.0             7.0           11
    ## 31       TRUE     TRUE            26.0            26.0           14
    ## 32      FALSE     TRUE            26.0            26.0           14
    ## 33      FALSE     TRUE            17.0            17.0           12
    ## 34      FALSE     TRUE            35.0            35.0           11
    ## 35       TRUE     TRUE            18.5            18.5           12
    ## 36       TRUE     TRUE            43.5            43.5           19
    ## 37       TRUE     TRUE            43.5            43.5           19
    ## 38      FALSE    FALSE              NA              NA           NA
    ## 39      FALSE     TRUE            40.5            40.5          114
    ## 40      FALSE     TRUE            40.5            40.5           13
    ## 41      FALSE     TRUE            40.5            40.5           13
    ## 42      FALSE     TRUE            40.5            40.5           13
    ## 43      FALSE     TRUE            54.0            54.0           42
    ## 44      FALSE     TRUE            40.5            40.5           23
    ## 45      FALSE     TRUE            43.5            43.5           26
    ## 46      FALSE    FALSE              NA              NA           NA
    ## 47      FALSE    FALSE              NA              NA           NA
    ## 48      FALSE    FALSE              NA              NA           NA
    ## 156        NA    FALSE              NA              NA           NA
    ## 210        NA    FALSE              NA              NA           NA
    ## 310        NA    FALSE              NA              NA           NA
    ## 410        NA    FALSE              NA              NA           NA
    ## 510        NA    FALSE              NA              NA           NA
    ## 610        NA    FALSE              NA              NA           NA
    ## 710        NA    FALSE              NA              NA           NA
    ## 84         NA    FALSE              NA              NA           NA
    ## 910        NA    FALSE              NA              NA           NA
    ## 1010       NA    FALSE              NA              NA           NA
    ## 1110       NA    FALSE              NA              NA           NA
    ## 1210       NA    FALSE              NA              NA           NA
    ## 1310       NA    FALSE              NA              NA           NA
    ## 1410       NA    FALSE              NA              NA           NA
    ## 157        NA    FALSE              NA              NA           NA
    ## 161        NA    FALSE              NA              NA           NA
    ## 171        NA    FALSE              NA              NA           NA
    ## 181        NA    FALSE              NA              NA           NA
    ## 191        NA    FALSE              NA              NA           NA
    ## 201        NA    FALSE              NA              NA           NA
    ## 211        NA    FALSE              NA              NA           NA
    ## 221        NA    FALSE              NA              NA           NA
    ## 231        NA    FALSE              NA              NA           NA
    ## 241        NA    FALSE              NA              NA           NA
    ## 251        NA    FALSE              NA              NA           NA
    ## 261        NA    FALSE              NA              NA           NA
    ## 271        NA    FALSE              NA              NA           NA
    ## 281        NA    FALSE              NA              NA           NA
    ## 291        NA    FALSE              NA              NA           NA
    ## 301        NA    FALSE              NA              NA           NA
    ## 311        NA    FALSE              NA              NA           NA
    ## 321        NA    FALSE              NA              NA           NA
    ## 351        NA    FALSE              NA              NA           NA
    ## 361        NA    FALSE              NA              NA           NA
    ## 371        NA    FALSE              NA              NA           NA
    ## 381        NA    FALSE              NA              NA           NA
    ## 391        NA    FALSE              NA              NA           NA
    ## 401        NA    FALSE              NA              NA           NA
    ## 411        NA    FALSE              NA              NA           NA
    ## 421        NA    FALSE              NA              NA           NA
    ## 431        NA    FALSE              NA              NA           NA
    ## 441        NA    FALSE              NA              NA           NA
    ## 451        NA    FALSE              NA              NA           NA
    ## 461        NA    FALSE              NA              NA           NA
    ## 471        NA    FALSE              NA              NA           NA
    ## 481        NA    FALSE              NA              NA           NA
    ## 49         NA    FALSE              NA              NA           NA
    ## 50         NA    FALSE              NA              NA           NA
    ## 51         NA    FALSE              NA              NA           NA
    ## 52         NA    FALSE              NA              NA           NA
    ## 53         NA    FALSE              NA              NA           NA
    ## 54         NA    FALSE              NA              NA           NA
    ## 55         NA    FALSE              NA              NA           NA
    ## 56         NA    FALSE              NA              NA           NA
    ## 57         NA    FALSE              NA              NA           NA
    ## 58         NA    FALSE              NA              NA           NA
    ## 59         NA    FALSE              NA              NA           NA
    ## 60         NA    FALSE              NA              NA           NA
    ## 61         NA    FALSE              NA              NA           NA
    ## 62         NA    FALSE              NA              NA           NA
    ## 63         NA    FALSE              NA              NA           NA
    ## 64         NA    FALSE              NA              NA           NA
    ## 65         NA    FALSE              NA              NA           NA
    ## 66         NA    FALSE              NA              NA           NA
    ## 67         NA    FALSE              NA              NA           NA
    ## 68         NA    FALSE              NA              NA           NA
    ## 69         NA    FALSE              NA              NA           NA
    ## 70         NA    FALSE              NA              NA           NA
    ## 71         NA    FALSE              NA              NA           NA
    ## 72         NA    FALSE              NA              NA           NA
    ## 73         NA    FALSE              NA              NA           NA
    ## 74         NA    FALSE              NA              NA           NA
    ## 75         NA    FALSE              NA              NA           NA
    ## 76         NA    FALSE              NA              NA           NA
    ## 77         NA    FALSE              NA              NA           NA
    ## 78         NA    FALSE              NA              NA           NA
    ## 79         NA    FALSE              NA              NA           NA
    ## 80         NA    FALSE              NA              NA           NA
    ## 81         NA    FALSE              NA              NA           NA
    ## 82         NA    FALSE              NA              NA           NA
    ## 83         NA    FALSE              NA              NA           NA
    ## 89         NA    FALSE              NA              NA           NA
    ## 90         NA    FALSE              NA              NA           NA
    ## 91         NA    FALSE              NA              NA           NA
    ## 92         NA    FALSE              NA              NA           NA
    ## 93         NA    FALSE              NA              NA           NA
    ## 94         NA    FALSE              NA              NA           NA
    ## 95         NA    FALSE              NA              NA           NA
    ## 96         NA    FALSE              NA              NA           NA
    ## 97         NA    FALSE              NA              NA           NA
    ## 98         NA    FALSE              NA              NA           NA
    ## 99         NA    FALSE              NA              NA           NA
    ## 100        NA    FALSE              NA              NA           NA
    ## 101        NA    FALSE              NA              NA           NA
    ## 102        NA    FALSE              NA              NA           NA
    ## 103        NA    FALSE              NA              NA           NA
    ## 104        NA    FALSE              NA              NA           NA
    ## 105        NA    FALSE              NA              NA           NA
    ## 106        NA    FALSE              NA              NA           NA
    ## 107        NA    FALSE              NA              NA           NA
    ## 108        NA    FALSE              NA              NA           NA
    ## 109        NA    FALSE              NA              NA           NA
    ## 110        NA    FALSE              NA              NA           NA
    ## 111        NA    FALSE              NA              NA           NA
    ## 112        NA    FALSE              NA              NA           NA
    ## 113        NA    FALSE              NA              NA           NA
    ## 114        NA    FALSE              NA              NA           NA
    ## 115        NA    FALSE              NA              NA           NA
    ## 116        NA    FALSE              NA              NA           NA
    ## 117        NA    FALSE              NA              NA           NA
    ## 118        NA    FALSE              NA              NA           NA
    ## 119        NA    FALSE              NA              NA           NA
    ## 120        NA    FALSE              NA              NA           NA
    ## 121        NA    FALSE              NA              NA           NA
    ## 122        NA    FALSE              NA              NA           NA
    ## 123        NA    FALSE              NA              NA           NA
    ## 124        NA    FALSE              NA              NA           NA
    ## 125        NA    FALSE              NA              NA           NA
    ## 126        NA    FALSE              NA              NA           NA
    ## 127        NA    FALSE              NA              NA           NA
    ## 128        NA    FALSE              NA              NA           NA
    ## 129        NA    FALSE              NA              NA           NA
    ## 130        NA    FALSE              NA              NA           NA
    ## 131        NA    FALSE              NA              NA           NA
    ## 132        NA    FALSE              NA              NA           NA
    ## 133        NA    FALSE              NA              NA           NA
    ## 134        NA    FALSE              NA              NA           NA
    ## 135        NA    FALSE              NA              NA           NA
    ## 136        NA    FALSE              NA              NA           NA
    ## 137        NA    FALSE              NA              NA           NA
    ## 138        NA    FALSE              NA              NA           NA
    ## 139        NA    FALSE              NA              NA           NA
    ## 140        NA    FALSE              NA              NA           NA
    ## 141        NA    FALSE              NA              NA           NA
    ## 142        NA    FALSE              NA              NA           NA
    ## 143        NA    FALSE              NA              NA           NA
    ## 144        NA    FALSE              NA              NA           NA
    ## 145        NA    FALSE              NA              NA           NA
    ## 146        NA    FALSE              NA              NA           NA
    ## 147        NA    FALSE              NA              NA           NA
    ## 148        NA    FALSE              NA              NA           NA
    ## 149        NA    FALSE              NA              NA           NA
    ## 150        NA    FALSE              NA              NA           NA
    ## 151        NA    FALSE              NA              NA           NA
    ## 152        NA    FALSE              NA              NA           NA
    ## 153        NA    FALSE              NA              NA           NA
    ## 154        NA    FALSE              NA              NA           NA
    ## 155        NA    FALSE              NA              NA           NA
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

test_that ("pkgstats", {

    expect_silent (s <- pkgstats (here::here ()))
    expect_type (s, "list")

    nms <- c ("cloc",
              "vignettes",
              "data_stats",
              "desc",
              "translations",
              "code_has_tabs",
              "objects",
              "network")
    expect_true (all (nms %in% names (s)))

    expect_s3_class (s$cloc, "tbl_df")
    expect_equal (nrow (s$cloc), 3L)
    expect_true (all (c ("R", "src", "tests") %in% s$cloc$source))

    expect_type (s$vignettes, "integer")
    expect_named (s$vignettes)
    expect_length (s$vignettes, 2L)

    expect_type (s$data_stats, "integer")
    expect_length (s$data_stats, 3L)
    expect_named (s$data_stats)

    expect_s3_class (s$desc, "data.frame")
    expect_equal (nrow (s$desc), 1L)
    expect_type (s$translations, "character")
    expect_type (s$code_has_tabs, "logical")
    expect_length (s$code_has_tabs, 1L)

    expect_s3_class (s$objects, "data.frame")
    #expect_true (nrow (s$objects) > 100L)
    nms <- c ("file_name",
              "fn_name",
              "kind",
              "language",
              "loc",
              "npars",
              "has_dots",
              "exported",
              "param_nchars_md",
              "param_nchars_mn",
              "num_doclines")
    expect_true (all (nms %in% names (s$objects)))

    expect_s3_class (s$network, "data.frame")
    #expect_true (nrow (s$network) > 50L)
    expect_true (nrow (s$network) < nrow (s$objects))
    nms <- c ("file",
              "line1",
              "from",
              "to",
              "language",
              "cluster_dir",
              "cluster_undir",
              "centrality_dir",
              "centrality_undir")
    #expect_true (all (nms %in% names (s$network)))
})

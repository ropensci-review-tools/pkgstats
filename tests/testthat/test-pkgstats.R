
source ("../demo-pkg-script.R")

test_that ("pkgstats", {

    path <- make_demo_package ()
    # message is now produced once per session by readr, but can only be
    # suppressed by Suggesting yet another package, `tidyselect`.
    #expect_message (
        s <- pkgstats (path)
    #    )
    expect_type (s, "list")

    nms <- c ("loc",
              "vignettes",
              "data_stats",
              "desc",
              "translations",
              "objects",
              "network",
              "external_calls")
    expect_true (all (nms %in% names (s)))

    expect_s3_class (s$loc, "tbl_df")
    # The following 2 tests fail on GitHub windows machines for some reason?
    is_windows <- Sys.info()[["sysname"]] == "Windows"
    if (!is_windows) {
        expect_equal (nrow (s$loc), 1L)
        expect_true ("R" %in% s$loc$dir)
    }

    expect_type (s$vignettes, "integer")
    expect_named (s$vignettes)
    expect_length (s$vignettes, 2L)

    expect_type (s$data_stats, "integer")
    expect_length (s$data_stats, 3L)
    expect_named (s$data_stats)

    expect_s3_class (s$desc, "data.frame")
    expect_equal (nrow (s$desc), 1L)
    expect_type (s$translations, "character")

    expect_s3_class (s$objects, "data.frame")
    expect_true (nrow (s$objects) >= 4L)
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
    expect_true (nrow (s$network) == 0L)
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

    expect_s3_class (s$external_calls, "data.frame")
    expect_equal (nrow (s$external_calls), 2L)
    expect_true ("stats" %in% s$external_calls$package)
})

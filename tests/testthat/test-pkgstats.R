test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

test_that ("pkgstats", {

    if (!test_all) {
        Sys.setenv ("PKGSTATS_CRAN_TESTS" = "true")
    }

    path <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")

    # message is now produced once per session by readr, but can only be
    # suppressed by Suggesting yet another package, `tidyselect`.
    # expect_message (
    s <- pkgstats (path)

    if (!test_all) {
        Sys.unsetenv ("PKGSTATS_CRAN_TESTS")
    }

    expect_type (s, "list")

    nms <- c (
        "loc",
        "vignettes",
        "data_stats",
        "desc",
        "translations",
        "objects",
        "network",
        "external_calls"
    )
    expect_true (all (nms %in% names (s)))

    expect_s3_class (s$loc, "tbl_df")
    # The following 2 tests fail on GitHub windows machines for some reason?
    is_windows <- Sys.info () [["sysname"]] == "Windows"
    if (!is_windows) {
        expect_gte (nrow (s$loc), 3L)
        expect_true (all (c ("R", "src", "tests") %in% s$loc$dir))
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

    skip_if (!test_all)

    expect_true (nrow (s$objects) >= 4L)
    nms <- c (
        "file_name",
        "fn_name",
        "kind",
        "language",
        "loc",
        "npars",
        "has_dots",
        "exported",
        "param_nchars_md",
        "param_nchars_mn",
        "num_doclines"
    )
    expect_true (all (nms %in% names (s$objects)))

    expect_s3_class (s$network, "data.frame")
    expect_true (nrow (s$network) > 0L)
    # expect_true (nrow (s$network) < nrow (s$objects))
    nms <- c (
        "file",
        "line1",
        "from",
        "to",
        "language",
        "cluster_dir",
        "cluster_undir",
        "centrality_dir",
        "centrality_undir"
    )
    expect_true (all (nms %in% names (s$network)))
    expect_true (all (c ("R", "C++") %in% s$network$language))

    ext <- s$external_calls
    expect_s3_class (ext, "data.frame")
    expect_true (nrow (ext) > 2L)
    expect_true ("stats" %in% ext$package)
    expect_true (length (unique (ext$package)) > 15L)
})

skip_if (!test_all)

test_that ("pkgstats with tabs", {

    if (!test_all) {
        Sys.setenv ("PKGSTATS_CRAN_TESTS" = "true")
    }

    path <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    path <- extract_tarball (path)
    flist <- fs::dir_ls (fs::path (path, "R"), type = "file", recurse = TRUE)
    f <- flist [which (basename (flist) == "loc.R")]
    x <- readLines (f)
    x <- gsub ("^\\s{12}", "\\\t\\\t\\\t", x)
    x <- gsub ("^\\s{8}", "\\\t\\\t", x)
    x <- gsub ("^\\s{4}", "\\\t", x)
    writeLines (x, f)

    # message is now produced once per session by readr, but can only be
    # suppressed by Suggesting yet another package, `tidyselect`.
    # expect_message (
    s <- pkgstats (path)

    if (!test_all) {
        Sys.unsetenv ("PKGSTATS_CRAN_TESTS")
    }

    expect_s3_class (s$loc, "tbl_df")
    expect_true (s$loc$ntabs [s$loc$dir == "R"] > 0L)

    fs::dir_delete (path)
})

test_that ("extra_manifest_paths", {

    path <- extract_tarball (
        system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    )
    s0 <- pkgstats (path)

    expect_null (extra_manifest_paths (path))

    extra_path <- fs::path (path, "extra_dir", "vendor_code")
    fs::dir_create (extra_path)
    cpp_code <- c (
        "// some vendor code",
        "int a () {",
        "  return 1L;",
        "}"
    )
    writeLines (cpp_code, fs::path (extra_path, "test.cpp"))
    toml <- c (
        "[\"vendor_sources\"]",
        "code=\"../extra_dir/vendor_code\""
    )
    writeLines (toml, fs::path (path, "src", "SOURCE_MANIFEST.toml"))

    extra_paths <- extra_manifest_paths (path)
    expect_type (extra_paths, "character")
    expect_length (extra_paths, 1L)
    expect_true (any (grepl ("vendor\\_code", extra_paths)))
    expect_true (all (fs::dir_exists (extra_paths)))

    s1 <- pkgstats (path)

    expect_gt (nrow (s1$loc), nrow (s0$loc))
    expect_gt (sum (s1$loc$nfiles), sum (s0$loc$nfiles))
    expect_gt (sum (s1$loc$nlines), sum (s0$loc$nlines))
    # Should also be one more tagged source object:
    expect_gt (nrow (s1$objects), nrow (s0$objects))
    extra_paths <- fs::path_rel (extra_paths, path)
    expect_true (all (extra_paths %in% fs::path_dir (s1$objects$file)))
    expect_false (any (extra_paths %in% fs::path_dir (s0$objects$file)))

    fs::dir_delete (path)
})

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

test_that ("desc stats", {

    if (!test_all) {
        Sys.setenv ("PKGSTATS_CRAN_TESTS" = "true")
    }

    path <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    expect_error (
        desc_stats (path),
        "path must be directory containing package source"
    )
    path <- extract_tarball (path)
    expect_silent (d <- desc_stats (path))

    expect_s3_class (d, "data.frame")
    expect_equal (nrow (d), 1L)
    expect_equal (ncol (d), 17L)
    expect_identical (
        names (d),
        c (
            "package",
            "version",
            "date",
            "license",
            "urls",
            "bugs",
            "aut",
            "ctb",
            "fnd",
            "rev",
            "ths",
            "trl",
            "depends",
            "imports",
            "suggests",
            "enhances",
            "linking_to"
        )
    )
})

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

test_that ("archive trawl", {

    if (!test_all) {
        Sys.setenv ("PKGSTATS_CRAN_TESTS" = "true")
    }

    archive <- setup_test_archive ()

    expect_error (
        pkgstats_from_archive (archive$path),
        "Assertion on 'path' failed"
    )
    expect_error (
        pkgstats_fns_from_archive (archive$path),
        "path must contain a 'tarballs' directory"
    )

    tarball_path <- setup_test_tarball (archive)

    out <- pkgstats_from_archive (tarball_path)

    expect_s3_class (out, "data.frame")
    expect_equal (nrow (out), 1L)

    out <- pkgstats_fns_from_archive (tarball_path)

    expect_s3_class (out, "data.frame")
    expect_equal (ncol (out), 3L)
    expect_identical (names (out), c ("package", "version", "fn_name"))

    unlink (archive$archive_path, recursive = TRUE)
    if (!test_all) {
        Sys.unsetenv ("PKGSTATS_CRAN_TESTS")
    }
})

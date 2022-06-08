
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

test_that ("archive trawl", {

    if (!test_all) {
        Sys.setenv ("PKGSTATS_CRAN_TESTS" = "true")
    }

    f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    tarball <- basename (f)

    archive_path <- file.path (tempdir (), "archive")
    if (!dir.exists (archive_path)) {
        dir.create (archive_path)
    }
    path <- file.path (archive_path, tarball)
    if (!file.exists (path)) {
        file.copy (f, path)
    }

    expect_error (
        pkgstats_from_archive (path),
        "path must contain a 'tarballs' directory"
    )
    expect_error (
        pkgstats_fns_from_archive (path),
        "path must contain a 'tarballs' directory"
    )

    tarball_path <- file.path (archive_path, "tarballs")
    if (!dir.exists (tarball_path)) {
        dir.create (tarball_path, recursive = TRUE)
    }
    if (!file.exists (file.path (tarball_path, tarball))) {
        file.copy (path, file.path (tarball_path, tarball))
    }

    out <- pkgstats_from_archive (tarball_path)

    expect_s3_class (out, "data.frame")
    expect_equal (nrow (out), 1L)

    out <- pkgstats_fns_from_archive (tarball_path)

    expect_s3_class (out, "data.frame")
    expect_equal (ncol (out), 3L)
    expect_identical (names (out), c ("package", "version", "fn_name"))

    unlink (archive_path, recursive = TRUE)
    if (!test_all) {
        Sys.unsetenv ("PKGSTATS_CRAN_TESTS")
    }
})

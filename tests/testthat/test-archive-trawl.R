test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

setup_test_archive <- function () {
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

    list (tarball = tarball, path = path, archive_path = archive_path)
}

setup_test_tarball <- function (archive) {
    tarball_path <- file.path (archive$archive_path, "tarballs")
    if (!dir.exists (tarball_path)) {
        dir.create (tarball_path, recursive = TRUE)
    }
    if (!file.exists (file.path (tarball_path, archive$tarball))) {
        file.copy (archive$path, file.path (tarball_path, archive$tarball))
    }

    return (tarball_path)
}

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

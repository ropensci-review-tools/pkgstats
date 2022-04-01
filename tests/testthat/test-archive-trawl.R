
test_that ("archive trawl", {

    f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    tarball <- utils::tail (fs::path_split (fs::path_tidy (f)) [[1]], 1L)

    dir.create (file.path (tempdir (), "archive"))
    archive_path <- file.path (tempdir (), "archive")
    path <- file.path (archive_path, tarball)
    file.copy (f, path)

    expect_error (
        pkgstats_from_archive (path),
        "path must contain a 'tarballs' directory"
    )

    tarball_path <- file.path (archive_path, "tarballs")
    dir.create (tarball_path, recursive = TRUE)
    file.copy (path, file.path (tarball_path, tarball))

    out <- pkgstats_from_archive (tarball_path)

    expect_s3_class (out, "data.frame")
    expect_equal (nrow (out), 1L)

    unlink (archive_path, recursive = TRUE)
})

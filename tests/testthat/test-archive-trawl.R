test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

test_that ("archive trawl", {

    if (!test_all) {
        Sys.setenv ("PKGSTATS_CRAN_TESTS" = "true")
    }

    archive <- setup_test_archive ()

    expect_type (archive, "list")
    expect_length (archive, 3L)
    expect_identical (names (archive), c ("tarball", "path", "archive_path"))

    v <- get_pkg_version (archive$path)
    expect_type (v, "character")
    expect_length (v, 2L)
    expect_identical (v, c ("pkgstats", "9.9"))

    f <- archive_results_file_name ("aaa.txt")
    expect_equal (basename (f), "aaa.Rds")
    expect_equal (f, fs::path (fs::path_wd (), "aaa.Rds"))

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

test_that ("archive_trawl with save options", {

    if (!test_all) {
        Sys.setenv ("PKGSTATS_CRAN_TESTS" = "true")
    }

    archive <- setup_test_archive ()
    rds_name <- gsub ("\\.tar\\.gz$", ".Rds", archive$tarball)
    tarball_path <- setup_test_tarball (archive)

    tempfiles <-
        basename (fs::dir_ls (fs::path_temp (), type = "file", recurse = TRUE))
    expect_false (rds_name %in% tempfiles)

    out <- pkgstats_from_archive (tarball_path, save_full = TRUE)
    tempfiles <-
        basename (fs::dir_ls (fs::path_temp (), type = "file", recurse = TRUE))
    expect_true (rds_name %in% tempfiles)

    rds_path <- fs::path (fs::path_temp (), rds_name)
    expect_true (fs::file_exists (rds_path))
    dat <- readRDS (rds_path)
    expect_type (dat, "list")
    expect_length (dat, 8L)
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
    expect_identical (names (dat), nms)
    fs::file_delete (rds_path)

    out <- pkgstats_from_archive (tarball_path, save_ex_calls = TRUE)
    tempfiles <- basename (fs::dir_ls (fs::path_temp (), type = "file"))
    expect_true (rds_name %in% tempfiles)

    dat <- readRDS (rds_path)
    expect_s3_class (dat, "data.frame")
    nms <- c (
        "tags_line", "call", "tag", "file", "kind", "start", "end", "package"
    )
    expect_equal (names (dat), nms)
    expect_equal (ncol (dat), 8L)
    if (test_all) {
        expect_true (nrow (dat) > 500L)
    }
    fs::file_delete (rds_path)

    unlink (archive$archive_path, recursive = TRUE)
    if (!test_all) {
        Sys.unsetenv ("PKGSTATS_CRAN_TESTS")
    }

})

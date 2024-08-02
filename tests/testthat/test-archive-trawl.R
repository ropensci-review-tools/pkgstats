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

test_that ("archive update errors", {

    msg <- "'prev_results' must be given"
    expect_error (pkgstats_update (), msg)
    expect_error (pkgstats_update ("a"), msg)
    expect_error (pkgstats_update (datasets::Orange), msg)
    dat <- null_stats ()
    expect_error (pkgstats_update (dat), msg)
    # param must have >= 20000 rows:
    dat <- dat [rep (1, 19999), ]
    expect_error (pkgstats_update (dat), msg)
    dat <- null_stats () [rep (1, 20000), -ncol (null_stats ())]
    expect_error (pkgstats_update (dat), msg)
    dat <- null_stats () [rep (1, 20000), ]
    names (dat) [1] <- "a"
    expect_error (pkgstats_update (dat), msg)
})

test_that ("archive update", {

    # Fake prev_results to contain all current CRAN packages minus one, and
    # expect that update will add the missing package. This will actually
    # download the package, so should only be run locally and on GitHub
    # runners.

    skip_if_not (test_all)

    archive <- setup_test_archive ()
    tarball_path <- setup_test_tarball (archive)

    cran_pkgs <- tools::CRAN_package_db ()
    index <- rep (1, times = nrow (cran_pkgs))
    prev_results <- null_stats () [index, ]
    prev_results$package <- cran_pkgs$Package
    prev_results$version <- cran_pkgs$Version

    i <- 1
    pkg_name <- prev_results$package [1]
    pkg_version <- prev_results$version [1]
    prev_results <- prev_results [-1, ]

    out1 <- pkgstats_update (
        prev_results = prev_results,
        num_cores = 1L,
        results_path = fs::path (fs::path_temp (), "pkgstats-results")
    )
    expect_equal (nrow (out1), nrow (prev_results) + 1)
    expect_true (pkg_name %in% out1$package)

    results_file <- fs::path (fs::path_temp (), "junk")
    out2 <- pkgstats_update (
        prev_results = prev_results,
        num_cores = 2L,
        results_path = fs::path (fs::path_temp (), "pkgstats-results"),
        results_file = results_file
    )
    expect_identical (out1, out2)
    expect_false (file.exists (results_file))
    expect_true (file.exists (paste0 (results_file, ".Rds")))

    unlink (archive$archive_path, recursive = TRUE)
    unlink (results_file)
})

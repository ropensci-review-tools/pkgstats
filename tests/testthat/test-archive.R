test_all <- (identical (Sys.getenv ("RRT_TEST_ALL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

test_that ("results file names with regex characters", {

    path <- fs::path (fs::path_temp (), "res(1)")
    fs::dir_create (path)
    f <- archive_results_file_name (fs::path (path, "stats(1)"))
    expect_equal (f, fs::path (expand_path (path), "stats(1).Rds"))

    path <- fs::path (fs::path_temp (), "out")
    fs::dir_create (path)
    f <- archive_results_file_name (fs::path (path, "out"))
    expect_equal (f, fs::path (expand_path (path), "out.Rds"))
})

test_that ("rbind_results drops errors", {

    res <- list (
        data.frame (a = 1),
        structure ("Error in f() : oops\n", class = "try-error"),
        NULL,
        data.frame (a = 2)
    )
    expect_warning (
        out <- rbind_results (res),
        "Dropped 1 results which errored: Error in f\\(\\) : oops"
    )
    expect_equal (out$a, c (1, 2))
})

test_that ("get_pkg_version on non-tarballs", {

    expect_identical (
        get_pkg_version (fs::path (fs::path_temp (), "pkgstats")),
        c (NA_character_, NA_character_)
    )
})

skip_if (!test_all)

test_that ("pkgstats_fns_from_archive with nothing new", {

    archive <- setup_test_archive ()
    tarball_path <- setup_test_tarball (archive)
    writeLines ("not a tarball", fs::path (tarball_path, "README.txt"))

    out <- pkgstats_fns_from_archive (tarball_path)
    expect_identical (unique (out$package), "pkgstats")

    out2 <- pkgstats_fns_from_archive (tarball_path, prev_results = out)
    expect_identical (out2, out)

    results_dir <- fs::path (fs::path_temp (), "fns(1)")
    fs::dir_create (results_dir)
    out3 <- pkgstats_fns_from_archive (
        tarball_path,
        prev_results = out,
        results_file = fs::path (results_dir, "fns(1)")
    )
    expect_true (fs::file_exists (fs::path (results_dir, "fns(1).Rds")))
    fs::dir_delete (results_dir)

    unlink (archive$archive_path, recursive = TRUE)
})

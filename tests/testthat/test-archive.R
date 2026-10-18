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

test_that ("rbind_results records failed packages", {

    paths <- c ("a_1.0.tar.gz", "b_2.0.tar.gz", "c_3.0.tar.gz", "d_4.0.tar.gz")
    ok <- failed_summary ("a_1.0.tar.gz")
    ok$loc_R <- 10L
    res <- list (
        ok,
        structure ("Error in f() : oops\n", class = "try-error"),
        NULL,
        failed_summary ("d_4.0.tar.gz")
    )
    expect_silent (out <- rbind_results (res, paths, failed_summary))
    expect_identical (out$package, c ("a", "b", "c", "d"))
    expect_identical (out$version, c ("1.0", "2.0", "3.0", "4.0"))
    expect_identical (out$loc_R, c (10L, NA, NA, NA))
    expect_identical (names (out), names (pkgstats_summary ()))

    res <- list (
        data.frame (package = "a", version = "1.0", fn_name = c ("f", "g")),
        structure ("Error in f() : oops\n", class = "try-error"),
        data.frame (package = character (0), version = character (0), fn_name = character (0)),
        NULL
    )
    out <- rbind_results (res, paths, failed_fn_names)
    expect_identical (out$package, c ("a", "a", "b", "c", "d"))
    expect_identical (out$fn_name, c ("f", "g", NA, NA, NA))
    expect_length (rm_prev_files (fs::path ("x", paths), out), 0L)
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

test_that ("failed archive packages are recorded and not retried", {

    archive <- setup_test_archive ()
    tarball_path <- setup_test_tarball (archive)
    broken <- fs::path (tarball_path, "broken_1.0.tar.gz")
    writeLines ("not a tarball", broken)

    out <- pkgstats_fns_from_archive (tarball_path)
    expect_true ("broken" %in% out$package)
    expect_true (all (is.na (out$fn_name [out$package == "broken"])))

    out2 <- pkgstats_fns_from_archive (tarball_path, prev_results = out)
    expect_identical (out2, out)

    summ <- pkgstats_from_archive (tarball_path)
    expect_true ("broken" %in% summ$package)
    expect_identical (summ$version [summ$package == "broken"], "1.0")
    expect_true (is.na (summ$loc_R [summ$package == "broken"]))

    unlink (archive$archive_path, recursive = TRUE)
})

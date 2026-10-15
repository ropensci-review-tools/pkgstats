test_all <- (identical (Sys.getenv ("RRT_TEST_ALL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

mk_tags <- function (tag, file, content, kind, start, end) {
    data.frame (
        tag = tag, file = file, content = content, kind = kind,
        start = start, end = end, stringsAsFactors = FALSE
    )
}

test_that ("local variables removed from calls in all files", {

    # 'df' is a local variable in both functions, which are in different
    # files, so start lines and rows of 'tags_r' differ for 'fb':
    tags_r <- rbind (
        mk_tags ("fa", "R/a.R", "fa <- function (x) {", "function", 1L, 3L),
        mk_tags ("df", "R/a.R", "    df <- data.frame (x)", "functionVar", 2L, 2L),
        mk_tags ("fb", "R/b.R", "fb <- function (y) {", "function", 10L, 12L),
        mk_tags ("df", "R/b.R", "    df <- head (y)", "functionVar", 11L, 11L)
    )
    path <- fs::path (fs::path_temp (), "localvars")
    fs::dir_create (fs::path (path, "R"))
    net <- external_call_network (tags_r, path, "testpkg")

    expect_false ("df" %in% net$call)
    expect_true (all (c ("data.frame", "head") %in% net$call))

    fs::dir_delete (path)
})

test_that ("calls to recommended packages", {

    # 'R CMD check --as-cran' hides recommended packages which are not
    # dependencies:
    rec <- c ("MASS", "rpart")
    skip_if (any (vapply (rec, function (p) {
        nrow (list_recommended_pkg_fns (p)) == 0L
    }, logical (1L))))

    calls <- data.frame (
        call = c ("rlm", "glmmPQL", "rpart", "not_a_fn"),
        package = NA_character_
    )
    out <- add_base_recommended_pkgs (calls)
    expect_identical (out$package, c ("MASS", "MASS", "rpart", NA_character_))
})

test_that ("imported names are not regular expressions", {

    path <- fs::path (fs::path_temp (), "imppkg")
    fs::dir_create (fs::path (path, "R"))
    writeLines (
        c ("importFrom(data.table, \"[.data.table\")", "importFrom(stats, sd)"),
        fs::path (path, "NAMESPACE")
    )
    writeLines ("f <- function (x) sd (x)", fs::path (path, "R", "f.R"))
    calls <- data.frame (
        tags_line = 1L, call = "f", tag = "f", file = "R/f.R",
        kind = "function", start = 1L, end = 1L, package = "imppkg"
    )

    out <- add_other_pkgs_to_calls (calls, path)
    expect_identical (out$package [out$call == "sd"], "stats")

    fs::dir_delete (path)
})

test_that ("doclines from ctags matched to function names", {

    fns <- data.frame (
        file_name = "R/f.R", fn_name = c ("a", "b", "c"), loc = 3L,
        npars = 1L, has_dots = FALSE, exported = FALSE,
        param_nchars_md = NA_integer_, param_nchars_mn = NA_integer_,
        num_doclines = NA_integer_
    )
    src <- data.frame (
        file_name = "R/f.R", fn_name = c ("b", "c", "a", "x"),
        kind = "function", language = "R", loc = 3L, npars = NA_integer_,
        has_dots = NA, num_doclines = c (20L, 30L, 10L, 99L)
    )
    out <- add_src_to_fn_data (fns, src)
    out <- out [out$language == "R", ]
    expect_identical (out$num_doclines, c (10L, 20L, 30L))
})

test_that ("network summary counts unique edges", {

    # 'a' calls 3 functions once each; 'b' calls 'c' three times:
    net <- data.frame (
        file = "R/a.R",
        from = c ("a", "a", "a", "b", "b", "b"),
        to = c ("b", "c", "d", "c", "c", "c"),
        cluster_dir = 1, centrality_dir = 1, centrality_undir = 1
    )
    s <- network_summary (net)
    expect_equal (s$n_edges, 4L)
    expect_equal (s$node_degree_max, 3L)
    expect_equal (s$node_degree_mn, 2)

    s0 <- network_summary (net [0L, ])
    expect_equal (s0$n_edges, 0L)
})

test_that ("packages without man directory", {

    path <- fs::path (fs::path_temp (), "nomanpkg")
    fs::dir_create (fs::path (path, "R"))
    writeLines (
        c ("Package: nomanpkg", "Version: 0.1", "Title: x", "License: GPL-3", "Description: x."),
        fs::path (path, "DESCRIPTION")
    )
    out_no_man <- rd_stats (path)
    expect_s3_class (out_no_man, "data.frame")
    # Same as for empty 'man' directory:
    fs::dir_create (fs::path (path, "man"))
    expect_identical (out_no_man, rd_stats (path))
    fs::dir_delete (path)
})

test_that ("rm_tabs removes tabs", {

    path <- fs::path (fs::path_temp (), "tabs", "R")
    fs::dir_create (path)
    writeLines (c ("f <- function (x) {", "\treturn (x)", "}"), fs::path (path, "f.R"))
    out <- rm_tabs (path)
    f <- fs::dir_ls (out, recurse = TRUE, type = "file")
    expect_false (any (grepl ("\\t", readLines (f))))
    # Original files are not modified:
    expect_true (any (grepl ("\\t", readLines (fs::path (path, "f.R")))))
    fs::dir_delete (fs::path_dir (path))
})

test_that ("fn names only from main NAMESPACE", {

    build <- fs::path (fs::path_temp (), "nmspbuild")
    path <- fs::path (build, "multins")
    fixture <- fs::path (path, "tests", "testthat", "testpkg")
    fs::dir_create (c (fs::path (path, "R"), fixture))
    desc <- c (
        "Package: multins", "Version: 1.0", "Title: x", "License: GPL-3",
        "Description: x.", "Author: a", "Maintainer: a <a@b.c>"
    )
    writeLines (desc, fs::path (path, "DESCRIPTION"))
    writeLines (c ("export(foo)", "export(bar)"), fs::path (path, "NAMESPACE"))
    writeLines (gsub ("multins", "testpkg", desc), fs::path (fixture, "DESCRIPTION"))
    writeLines ("export(fixture_fn)", fs::path (fixture, "NAMESPACE"))
    writeLines (c ("foo <- function () 1", "bar <- function () 2"), fs::path (path, "R", "f.R"))
    tarball <- fs::path (fs::path_temp (), "multins_1.0.tar.gz")
    withr::with_dir (build, utils::tar (tarball, "multins", compression = "gzip", tar = "internal"))

    for (p in c (tarball, path)) {
        out <- pkgstats_fn_names (p)
        expect_identical (sort (as.character (out$fn_name)), c ("bar", "foo"))
        expect_identical (unique (out$package), "multins")
    }

    # Directory without NAMESPACE:
    fs::file_delete (fs::path (path, "NAMESPACE"))
    expect_null (get_namespace_contents (path))

    fs::dir_delete (build)
    fs::file_delete (tarball)
})

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

test_that ("ctags lines with quotes", {

    path <- fs::path (fs::path_temp (), "quotepkg")
    fs::dir_create (fs::path (path, "R"))
    writeLines (
        c ("f <- function (x) {", "    g (x)", "}", "g <- function (y) y"),
        fs::path (path, "R", "f.R")
    )
    tags <- withr::with_dir (path, get_ctags ("R", has_tabs = FALSE))
    expect_true (all (c ("f", "g") %in% tags$tag))
    fs::dir_delete (path)
})

test_that ("summary column types match dictionary", {

    f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    s <- pkgstats_summary (pkgstats (f))
    dict <- read_col_types ()
    ok <- vapply (seq_len (nrow (dict)), function (i) {
        col_type_ok (s [[dict$name [i]]], dict$type [i])
    }, logical (1L))
    expect_true (all (ok))
    expect_identical (names (s), names (pkgstats_summary ()))
})

test_that ("pkgstats_fns_from_archive with nothing new", {

    archive <- setup_test_archive ()
    tarball_path <- setup_test_tarball (archive)
    writeLines ("not a tarball", fs::path (tarball_path, "README.txt"))

    out <- pkgstats_fns_from_archive (tarball_path)
    expect_identical (unique (out$package), "pkgstats")

    out2 <- pkgstats_fns_from_archive (tarball_path, prev_results = out)
    expect_identical (out2, out)

    # 'results_file' with regex characters:
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

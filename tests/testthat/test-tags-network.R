test_all <- (identical (Sys.getenv ("RRT_TEST_ALL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

mk_tags <- function (tag, file, content, kind, start, end) {
    data.frame (
        tag = tag, file = file, content = content, kind = kind,
        start = start, end = end, stringsAsFactors = FALSE
    )
}

test_that ("local variables removed from calls in all files", {

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

test_that ("network summary counts unique edges", {

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

    net$file <- c ("R/a.R", "R/a.R", "src/a.c", "R/a.R", "R/a.R", "R/a.R")
    net$centrality_dir <- c (0, 1, 0, 1, 1, 1)
    s <- network_summary (net)
    expect_equal (s$n_edges, 4L)
    expect_equal (s$n_edges_r + s$n_edges_src, s$n_edges)
    expect_equal (s$n_edges_src, 1L)
    expect_equal (s$num_terminal_edges_dir, 2L)

    s0 <- network_summary (net [0L, ])
    expect_equal (s0$n_edges, 0L)
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

test_that ("gtags from extra manifest paths", {

    path <- extract_tarball (
        system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    )
    extra_path <- fs::path (path, "extra_dir", "vendor_code")
    fs::dir_create (extra_path)
    cpp_code <- c (
        "int vendor_a () {",
        "  return 1;",
        "}",
        "",
        "int vendor_b () {",
        "  return vendor_a ();",
        "}"
    )
    writeLines (cpp_code, fs::path (extra_path, "vendor.cpp"))
    toml <- c (
        "[\"vendor_sources\"]",
        "code=\"../extra_dir/vendor_code\""
    )
    writeLines (toml, fs::path (path, "src", "SOURCE_MANIFEST.toml"))

    tags <- tags_data (path)
    net <- tags$network [which (tags$network$to == "vendor_a"), ]
    expect_true (nrow (net) > 0L)
    expect_true (all (net$file == "extra_dir/vendor_code/vendor.cpp"))
    expect_false (any (fs::file_exists (fs::path (extra_path, c ("GTAGS", "GRTAGS", "GPATH")))))

    fs::dir_delete (path)
})

test_that ("gtags with a single reference line", {

    skip_if (!nzchar (Sys.which ("global")) || !nzchar (Sys.which ("gtags")))

    path <- withr::local_tempdir ()
    writeLines (
        c ("int f (void) { return 1; }", "int g (void) { return f (); }"),
        fs::path (path, "a.c")
    )
    withr::with_dir (path, make_gtags ())
    g <- withr::with_dir (path, get_gtags ())
    expect_equal (nrow (g), 1L)
    expect_equal (g$tag, "f")
})

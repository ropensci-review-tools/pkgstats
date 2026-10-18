test_all <- (identical (Sys.getenv ("RRT_TEST_ALL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

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

    fns$file_name <- c ("R/f.R", "R/f.R", "R/g.R")
    fns$fn_name <- c ("a", "b", "a")
    src <- data.frame (
        file_name = c ("R/g.R", "R/f.R", "R/f.R"), fn_name = c ("a", "a", "b"),
        kind = "function", language = "R", loc = 3L, npars = NA_integer_,
        has_dots = NA, num_doclines = c (7L, 10L, 20L)
    )
    out <- add_src_to_fn_data (fns, src)
    out <- out [out$language == "R", ]
    expect_identical (out$num_doclines, c (10L, 20L, 7L))
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

    fs::file_delete (fs::path (path, "NAMESPACE"))
    expect_null (get_namespace_contents (path))

    writeLines (c ("export(foo)", "export(bar)"), fs::path (path, "NAMESPACE"))
    fs::file_delete (fs::path (path, "DESCRIPTION"))
    expect_null (get_desc_path (path))
    expect_identical (get_pkg_name_version (NULL), c (NA_character_, NA_character_))

    nodesc <- fs::path (fs::path_temp (), "nodesc_1.0.tar.gz")
    withr::with_dir (build, utils::tar (nodesc, "multins", compression = "gzip", tar = "internal"))
    expect_null (get_desc_path (nodesc))
    fs::file_delete (nodesc)

    fs::dir_delete (build)
    fs::file_delete (tarball)
})

skip_if (!test_all)

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

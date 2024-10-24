test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

skip_if (!test_all)

test_that ("cran data update functions", {

    results_path <- fs::path (fs::path_temp (), "pkgstats-data")
    fs::dir_create (results_path)

    f <- dl_prev_data (results_path, what = "current")
    expect_type (f, "character")
    expect_length (f, 1L)
    expect_true (fs::file_exists (f))

    x <- readRDS (f)
    expect_s3_class (x, "data.frame")
    expect_equal (ncol (x), 95L)
    expect_true (nrow (x) > 20000L)

    expect_silent (
        check_prev_results (x)
    )

    new <- list_new_cran_updates (x)
    expect_type (new, "character")
    if (length (new) > 0L) {
        expect_true (all (grepl ("\\_[0-9]", new)))
    }

    fs::dir_delete (results_path)
})

test_that ("download tarball", {

    pkg <- "odbc"
    u <- paste0 ("https://cran.r-project.org/package=", pkg)
    qry <- httr2::request (u)
    resp <- httr2::req_perform (qry)
    # No 'xml' or 'rvest' here, so process as raw text:
    b <- httr2::resp_body_string (resp)
    b <- strsplit (b, ",\\s") [[1]]
    tarball <- grep ("\\.tar\\.gz", b, value = TRUE)
    ptn <- "odbc\\_[0-9]\\.[0-9]\\.[0-9]\\.tar\\.gz"
    tarball <- regmatches (tarball, regexpr (ptn, tarball))
    tarball <- gsub ("\\.tar\\.gz$", "", tarball)

    results_path <- fs::path (fs::path_temp (), "pkgstats-data")
    fs::dir_create (results_path)
    flist <- fs::dir_ls (results_path, type = "file")

    path <- dl_one_tarball (results_path, tarball)
    expect_true (fs::file_exists (path))

    fs::dir_delete (results_path)
})

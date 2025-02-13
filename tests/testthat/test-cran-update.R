test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

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

    path <- dl_one_tarball (results_path, tarball)
    expect_true (fs::file_exists (path))

    fs::dir_delete (results_path)
})

# Some pkgs define an "exportPattern" in the namespace, and use that to define
# exported functions. These call the separate `aliases_from_rd()` function,
# which is covered in these tests. The selected package was identified using
# the following code:
#
# flist <- fs::dir_ls (path_to_tarballs, regexp = "\\.tar\\.gz$")
# dat <- pbapply::pblapply (flist, function (f) {
#
#     tb <- tryCatch (extract_tarball (f), error = function (e) NULL)
#     ret <- NULL
#     if (!is.null (tb)) {
#         nmsp <- get_namespace_contents (tb)
#         if (any (grepl ("exportPattern", nmsp))) {
#             ret <- c (unname (f), as.character (file.size (f)))
#         }
#         fs::dir_delete (tb)
#     }
#
#     return (ret)
# })
# dat <- do.call (rbind, dat)
# rownames (dat) <- NULL
# dat <- data.frame (pkg = basename (dat [, 1]), size = as.numeric (dat [, 2]))
# dat <- dat [order (dat$size), ]
#
test_that ("fn_names_from_export_ptn", {

    pkg <- "abodOutlier"
    u <- paste0 ("https://cran.r-project.org/package=", pkg)
    qry <- httr2::request (u)
    resp <- httr2::req_perform (qry)
    # No 'xml' or 'rvest' here, so process as raw text:
    b <- httr2::resp_body_string (resp)
    b <- strsplit (b, ",\\s") [[1]]
    tarball <- grep ("\\.tar\\.gz", b, value = TRUE)
    ptn <- "abodOutlier\\_[0-9]\\.[0-9]\\.tar\\.gz"
    tarball <- regmatches (tarball, regexpr (ptn, tarball))
    tarball <- gsub ("\\.tar\\.gz$", "", tarball)

    results_path <- fs::path (fs::path_temp (), "pkgstats-data")
    fs::dir_create (results_path)

    path <- dl_one_tarball (results_path, tarball)
    expect_true (fs::file_exists (path))

    fn_nms <- pkgstats_fn_names (path)
    expect_s3_class (fn_nms, "data.frame")
    expect_equal (ncol (fn_nms), 3L)
    expect_equal (names (fn_nms), c ("package", "version", "fn_name"))
    expect_true (nrow (fn_nms) > 0L)

    fs::dir_delete (results_path)

    pkg <- fs::path (fs::path_temp (), gsub ("\\_.*$", "", tarball))
    if (fs::dir_exists (pkg)) {
        fs::dir_delete (pkg)
    }

})

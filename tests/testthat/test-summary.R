
test_all <- (
    identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
        identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage")
)

test_that ("pkgstats-null-summary", {

    path <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    p <- pkgstats (path)
    s1 <- pkgstats_summary (p)

    s0 <- pkgstats_summary (NULL)
    expect_equal (ncol (s0), ncol (s1))

    expect_identical (names (s0), names (s1))
})

test_that ("pkgstats-summary", {

    path <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")

    p <- pkgstats (path)

    expect_silent (
        s <- pkgstats_summary (p)
    )

    expect_s3_class (s, "data.frame")
    expect_equal (nrow (s), 1L)
    expect_equal (ncol (s), 92L)

    skip_if (!test_all)

    # external_calls:
    ext <- strsplit (strsplit (s$external_calls, ",") [[1]], ":")
    ext <- do.call (rbind, ext)
    expect_equal (ncol (ext), 3L)
    expect_true (nrow (ext) > 15L) # 17 packages

    ext <- data.frame (
        pkg = ext [, 1],
        n_total = as.integer (ext [, 2]),
        n_unique = as.integer (ext [, 3])
    )
    expect_true (all (ext$n_total >= ext$n_unique))
    expect_true (mean (ext$n_total) > mean (ext$n_unique))
})

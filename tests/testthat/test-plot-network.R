test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

skip_if (!test_all)

test_that ("plot-network", {

    path <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    s <- pkgstats (path)

    requireNamespace ("visNetwork")
    expect_silent (
        net0 <- plot_network (s, plot = FALSE)
    )

    expect_s3_class (net0, "visNetwork")
    expect_type (net0, "list")
    expect_true (length (net0) > 1L)

    nodes0 <- net0$x$nodes
    edges0 <- net0$x$edges
    expect_equal (ncol (nodes0), 7L)
    expect_equal (ncol (edges0), 6L)
    expect_gt (nrow (nodes0), 50L)
    expect_gt (nrow (edges0), 50L)

    expect_silent (
        net1 <- plot_network (s, fn = "cpp_loc", plot = FALSE)
    )
    nodes1 <- net1$x$nodes
    edges1 <- net1$x$edges
    expect_equal (ncol (nodes0), ncol (nodes1))
    expect_equal (ncol (edges0), ncol (edges1))
    expect_lt (nrow (nodes1), 5L)
    expect_lt (nrow (edges1), 5L)
})

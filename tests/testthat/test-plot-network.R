test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

skip_if (!test_all)

test_that ("plot-network", {

    path <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    s <- pkgstats (path)

    expect_silent (
        net <- plot_network (s, plot = FALSE)
    )

    expect_s3_class (net, "pkgstats_network")
    expect_type (net, "list")
    expect_true (length (net) > 1L)

    nodes0 <- net$nodes
    edges0 <- net$edges
    expect_equal (ncol (nodes0), 8L)
    expect_equal (ncol (edges0), 6L)
    expect_gt (nrow (nodes0), 50L)
    expect_gt (nrow (edges0), 50L)

    expect_type (net$html, "character")
    expect_true (grepl ("pkgstatsNetworkPlot", net$html, fixed = TRUE))
})

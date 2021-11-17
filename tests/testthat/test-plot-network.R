
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

skip_if (!test_all)

test_that ("plot-network", {

    path <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    s <- pkgstats (path)

    requireNamespace ("visNetwork")
    expect_silent (
       net <- plot_network (s, plot = FALSE)
       )

    expect_s3_class (net, "visNetwork")
    expect_type (net, "list")
    expect_true (length (net) > 1L)
})

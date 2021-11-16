
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

skip_if (!test_all)

source ("../demo-pkg-script.R")

test_that ("plot-network", {

    path <- make_demo_package ()
    s <- pkgstats (path)

    # This produces a message in non-interactive mode:
    # expect_silent (
    net <- plot_network (s, plot = FALSE)
    #    )

    expect_s3_class (net, "visNetwork")
    expect_type (net, "list")
    expect_true (length (net) > 1L)
})

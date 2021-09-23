
source ("../demo-pkg-script.R")

test_that ("pkgstats-summary", {

    path <- make_demo_package ()

    p <- pkgstats (path)

    expect_silent (
        s <- pkgstats_summary (p)
        )

    expect_s3_class (s, "data.frame")
    expect_equal (nrow (s), 1L)
    expect_equal (ncol (s), 91L)
})


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

    # external_calls:
    ext <- strsplit (strsplit (s$external_calls, ",") [[1]], ":")
    ext <- do.call (rbind, ext)
    expect_equal (ncol (ext), 3L)
    expect_equal (nrow (ext), 2L) # 2 packages

    ext <- data.frame (pkg = ext [, 1],
                       n_total = as.integer (ext [, 2]),
                       n_unique = as.integer (ext [, 3]))
    expect_true (all (ext$n_total >= ext$n_unique))
    expect_true (mean (ext$n_total) > mean (ext$n_unique))
})

test_that ("fn_names", {

    path <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    s <- pkgstats_fn_names (path)
    expect_s3_class (s, "data.frame")
    expect_equal (ncol (s), 3L)
    expect_named (s, c ("package", "version", "fn_name"))

    expect_type (s$package, "character")
    expect_type (s$version, "character")
    expect_type (s$fn_name, "character")
    expect_equal (class (s$fn_name), "noquote")

    expect_length (unique (s$package), 1L)
    expect_length (unique (s$version), 1L)
    expect_length (unique (s$fn_name), nrow (s))
})

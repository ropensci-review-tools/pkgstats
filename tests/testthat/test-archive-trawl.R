
source ("../demo-pkg-script.R")

test_that ("archive trawl", {

    pkg <- make_demo_package ()

    path <- decompose_path (pkg) [[1]] [-1]
    pkg_name <- path [length (path)]
    path <- do.call (file.path, as.list (path [-length (path)]))

    expect_error (
        pkgstats_from_archive (path),
        "path must contain a 'tarballs' directory"
    )

    path <- file.path (path, "tarballs")
    dir.create (path, recursive = TRUE)
    file.copy (pkg, file.path (path, pkg_name))

    out <- pkgstats_from_archive (path)

    expect_s3_class (out, "data.frame")
    expect_equal (nrow (out), 1L)
})

test_that ("manifest paths", {

    exdir <- fs::path (fs::path_temp (), "pkgroot", "subdir")
    path <- extract_tarball (
        system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats"),
        exdir = exdir
    )
    extra_path <- fs::path (path, "extra_dir", "vendor_code")
    fs::dir_create (extra_path)

    check_path (exdir)
    check_path (fs::path_dir (exdir))
    check_path (extra_path)

    fs::dir_delete (path)
    expect_error (
        check_path (exdir),
        "Could not find unambiguous project root"
    )

    fs::dir_delete (exdir)
})

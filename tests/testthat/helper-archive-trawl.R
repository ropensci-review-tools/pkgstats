setup_test_archive <- function () {
    f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    tarball <- basename (f)

    archive_path <- fs::path (fs::path_temp (), "archive")
    if (!fs::dir_exists (archive_path)) {
        fs::dir_create (archive_path)
    }
    path <- fs::path (archive_path, tarball)
    if (!fs::file_exists (path)) {
        fs::file_copy (f, path)
    }

    list (tarball = tarball, path = path, archive_path = archive_path)
}

setup_test_tarball <- function (archive) {
    tarball_path <- fs::path (archive$archive_path, "tarballs")
    if (!fs::dir_exists (tarball_path)) {
        fs::dir_create (tarball_path, recurse = TRUE)
    }
    if (!fs::file_exists (fs::path (tarball_path, archive$tarball))) {
        fs::file_copy (archive$path, fs::path (tarball_path, archive$tarball))
    }

    return (tarball_path)
}

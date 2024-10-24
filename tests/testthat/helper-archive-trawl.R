setup_test_archive <- function () {
    f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    tarball <- basename (f)

    archive_path <- file.path (tempdir (), "archive")
    if (!dir.exists (archive_path)) {
        dir.create (archive_path)
    }
    path <- file.path (archive_path, tarball)
    if (!file.exists (path)) {
        file.copy (f, path)
    }

    list (tarball = tarball, path = path, archive_path = archive_path)
}

setup_test_tarball <- function (archive) {
    tarball_path <- file.path (archive$archive_path, "tarballs")
    if (!dir.exists (tarball_path)) {
        dir.create (tarball_path, recursive = TRUE)
    }
    if (!file.exists (file.path (tarball_path, archive$tarball))) {
        file.copy (archive$path, file.path (tarball_path, archive$tarball))
    }

    return (tarball_path)
}

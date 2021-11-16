#' Extract tarball of a package into temp directory and return path to extracted
#' package
#'
#' @param tarball Full path to local tarball of an R package.
#' @return Path to extracted version of package (in `tempdir()`).
#' @family misc
#' @export
#' @examples
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' path <- extract_tarball (f)
extract_tarball <- function (tarball) {

    if (!file.exists (tarball)) {
        stop ("file [", tarball, "] does not exist")
    }
    if (!checkmate::testCharacter (tarball,
        len = 1,
        pattern = "\\.tar\\.gz$"
    )) {
        stop (paste0 (
            "tarball must be a single character ",
            "specifying path to .tar.gz file"
        ))
    }

    flist <- utils::untar (tarball,
        exdir = tempdir (),
        list = TRUE, tar = "internal"
    )
    if (utils::untar (tarball, exdir = tempdir (), tar = "internal") != 0) {
        stop ("Unable to extract tarball to 'tempdir'")
    }

    fdir <- vapply (flist, function (i) {
        strsplit (i, .Platform$file.sep) [[1]] [1]
    },
    character (1),
    USE.NAMES = FALSE
    )
    fdir <- names (table (fdir)) [1]
    path <- normalizePath (file.path (tempdir (), fdir))

    chk <- rename_files_in_r (path)
    if (!chk) {
        warning ("Files in .R directory unable to be re-named")
    }

    return (path)
}

#' files in './R' directory may have suffixes of .q, .r, or .s
#' https://cran.r-project.org/doc/manuals/R-exts.html#Package-subdirectories
#' But parsers + cloc only recognise .R/.r, as does gtags, so rename any others
#' Discovered via BDR's
#' https://cran.r-project.org/web/packages/boot/index.html
#' The `rms` package also has loads of '.s' code, which cloc's dictionary
#' identifies as assembly
#' @noRd
rename_files_in_r <- function (path) {

    fr <- normalizePath (list.files (file.path (path, "R"),
        full.names = TRUE
    ))

    index <- grep ("\\.(s|S|q)$", fr, ignore.case = TRUE)

    chk <- TRUE

    if (length (index) > 0) {

        for (f in fr [index]) {

            chk <- c (
                chk,
                file.rename (f, gsub ("\\.[a-zA-Z]$", ".R", f))
            )
        }
    }

    return (all (chk))
}

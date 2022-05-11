
#' Extract names of all functions for one R package
#'
#' @inheritParams pkgstats
#' @return A `data.frame` with two columns:
#' \itemize{
#' \item Name of package
#' \item Name of function
#' }
#' @family misc
#' @export
pkgstats_fn_names <- function (path) {

    path <- check_path (path)

    tarball <- grepl ("\\.tar\\.gz$", path)

    if (tarball) {

        flist <- utils::untar (
            path,
            exdir = tempdir (),
            list = TRUE,
            tar = "internal"
        )

        nmsp <- grep ("NAMESPACE", flist, value = TRUE)
        # Some archived pkgs do not have NAMESPACE files; these are not
        # processed here:
        if (length (nmsp) == 0L) {
            return (NULL)
        }

        chk <- utils::untar (
            path,
            files = nmsp,
            exdir = tempdir ()
        )

        if (chk != 0) {
            stop ("Unable to extract tarball [", tarball, "]",
                call. = FALSE
            )
        }

        nmsp <- file.path (tempdir (), nmsp)

        desc <- grep ("DESCRIPTION", flist, value = TRUE)
        chk <- utils::untar (
            path,
            files = desc,
            exdir = tempdir ()
        )
        desc <- normalizePath (file.path (tempdir (), desc))

    } else {

        nmsp <- list.files (
            path,
            recursive = TRUE,
            full.names = TRUE,
            pattern = "NAMESPACE"
        )
        nmsp <- normalizePath (nmsp [1])

        desc <- list.files (
            path,
            recursive = TRUE,
            full.names = TRUE,
            pattern = "DESCRIPTION"
        )
        desc <- normalizePath (desc [1])
    }

    # See R source in src/library/base/R/namespace.R for reference, especially
    # the `parseNamespaceFile()` function.
    nmsp <- parse (
        nmsp,
        keep.source = FALSE,
        srcfile = NULL
    )

    if (any (grepl ("^exportPattern", nmsp))) {

        fns <- names_from_rd (path, tarball)

    } else {

        fns <- grep ("^export\\s?\\(", nmsp, value = TRUE)
        fns <- gsub ("^export\\s?\\(|\\)\\s?$", "", fns)
        fns <- gsub ("\\#.*$|\\\t", "", fns)
        fns <- unlist (strsplit (fns, ","))
        fns <- gsub ("^\\s*|\\s*$", "", fns)
    }

    # Then get imports to remove re-exported fns:
    imps <- grep ("^importFrom", nmsp, value = TRUE)
    imps <- gsub ("^importFrom\\(|\\)\\s?$", "", imps)
    imps <- gsub ("^[^,]*,", "", imps)
    imps <- gsub (
        "^\\s*|\\s*$", "",
        unlist (strsplit (imps, ","))
    )

    fns <- fns [which (!fns %in% imps)]

    pkg <- get_pkg_name_version (desc)

    data.frame (
        package = pkg [1],
        version = pkg [2],
        fn_name = fns,
        stringsAsFactors = FALSE
    )
}

names_from_rd <- function (path, tarball) {

    if (tarball) {

        flist <- utils::untar (
            path,
            exdir = tempdir (),
            list = TRUE,
            tar = "internal"
        )

        rd <- grep ("\\.Rd$", flist, value = TRUE)

        chk <- utils::untar (
            path,
            files = rd,
            exdir = tempdir ()
        )

        flist <- file.path (tempdir (), rd)

    } else {

        flist <- list.files (
            path,
            pattern = "\\.Rd$",
            recursive = TRUE,
            full.names = TRUE
        )
    }

    flist <- normalizePath (flist)
    # only extract Rd files from man directory:
    is_man <- vapply (
        fs::path_split (flist),
        function (i) {
            any (i == "man")
        },
        logical (1L)
    )
    flist <- flist [which (is_man)]

    if (chk != 0) {
    }

    nms <- lapply (flist, function (i) {

        rd_i <- tools::parse_Rd (i)

        docType <- get_Rd_metadata (rd_i, "docType")
        docType <- ifelse (length (docType) == 0L, "", docType)

        out <- NULL

        if (!docType %in% c ("class", "data", "methods", "package")) {
            out <- unique (c (
                get_Rd_metadata (rd_i, "name"),
                get_Rd_metadata (rd_i, "alias")
            ))
            index <- grep ("method(s?|,?)$|class$|<\\-|\\[\\[\\,|\\s", out)
            if (length (index) > 0L) {
                out <- out [-index]
            }
            methods <- unlist (lapply (rd_i, function (j) {
                get_Rd_metadata (j, "method")
            }))
            if (length (methods) > 0L) {
                out <- NULL
            }
        }

        if (any (grepl ("\\[\\[|\\$|<\\-|,|\\s", out))) {
                message (i)
            }
        return (out)
    })

    return (unique (unlist (nms)))
}

get_pkg_name_version <- function (desc) {

    d <- data.frame (read.dcf (desc))

    return (c (d$Package, d$Version))
}

#' Extract names of all functions for one R package
#'
#' @inheritParams pkgstats
#' @return A `data.frame` with three columns:
#' \itemize{
#' \item package: Name of package
#' \item version: Package version
#' \item fn_name: Name of function
#' }
#' @family misc
#' @export
#' @examples
#' # 'path' can be path to a package tarball:
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' path <- extract_tarball (f)
#' s <- pkgstats_fn_names (path)
pkgstats_fn_names <- function (path) {

    path <- check_path (path)

    nmsp <- get_namespace_contents (path)

    desc_path <- get_desc_path (path)

    if (any (grepl ("^exportPattern", nmsp))) {

        fns <- aliases_from_rd (path, nmsp)

    } else if (!any (grepl ("^export", nmsp))) {

        # no functions exported
        return (data.frame (
            package = character (0L),
            version = character (0L),
            fn_name = character (0L),
            stringsAsFactors = FALSE
        ))

    } else {

        fns <- grep ("^export\\s?\\(", nmsp, value = TRUE)
        fns <- gsub ("^export\\s?\\(|\\)\\s?$", "", fns)
        fns <- gsub ("\\#.*$|\\\t", "", fns)
        fns <- unlist (strsplit (fns, ","))
        fns <- gsub ("^\\s*|\\s*$", "", fns)

        # same grep as for aliases_from_rd below:
        index <- grep ("method(s?|,?)$|class$|<\\-|\\[\\[|,|\\s", fns)
        if (length (index) > 0) {
            fns <- fns [-index]
        }
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
    if (!is.null (fns)) {
        fns <- noquote (fns)
    }

    pkg <- get_pkg_name_version (desc_path)

    data.frame (
        package = rep (pkg [1], length (fns)),
        version = rep (pkg [2], length (fns)),
        fn_name = gsub ("^\\\"|\\\"$", "", fns),
        stringsAsFactors = FALSE
    )
}

get_namespace_contents <- function (path) {

    tarball <- grepl ("\\.tar\\.gz$", path)

    if (tarball) {

        flist <- utils::untar (
            path,
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
            exdir = fs::path_temp ()
        )

        if (chk != 0) {
            stop ("Unable to extract tarball [", tarball, "]",
                call. = FALSE
            )
        }

        nmsp <- fs::path (fs::path_temp (), nmsp)

        on.exit ({
            fs::dir_delete (fs::path_dir (nmsp))
        })

    } else {

        nmsp <- fs::dir_ls (
            path,
            recurse = TRUE,
            regexp = "NAMESPACE"
        )
        nmsp <- expand_path (nmsp [1])

    }

    # See R source in src/library/base/R/namespace.R for reference, especially
    # the `parseNamespaceFile()` function.
    nmsp_parsed <- parse (
        nmsp,
        keep.source = FALSE,
        srcfile = NULL
    )

    return (nmsp_parsed)
}

get_desc_path <- function (path) {

    tarball <- grepl ("\\.tar\\.gz$", path)

    if (tarball) {

        flist <- utils::untar (
            path,
            exdir = fs::path_temp (),
            list = TRUE,
            tar = "internal"
        )

        desc <- grep ("DESCRIPTION", flist, value = TRUE)
        chk <- utils::untar (
            path,
            files = desc,
            exdir = fs::path_temp ()
        )
        desc <- expand_path (fs::path (fs::path_temp (), desc))

    } else {

        desc <- fs::dir_ls (
            path,
            recurse = TRUE,
            regexp = "DESCRIPTION"
        )
        desc <- expand_path (desc [1])
    }

    return (desc)
}

#' List all functions defined in R source code which match 'exportPattern'
#'
#' @noRd
exp_ptn_r_fn_names <- function (path, nmsp) {

    tarball <- grepl ("\\.tar\\.gz$", path)

    if (tarball) {

        path_dir <- extract_tarball (path)
        s1 <- loc_stats (path_dir)
        has_tabs <- any (s1$ntabs > 0L)
        tags_r <- withr::with_dir (path_dir, get_ctags ("R", has_tabs))

    } else {

        s1 <- loc_stats (path)
        has_tabs <- any (s1$ntabs > 0L)
        tags_r <- withr::with_dir (path, get_ctags ("R", has_tabs))

    }

    tags_r <- tags_r [which (tags_r$kind == "function"), ]
    exp_ptn <- gsub (
        "^exportPattern\\(|\\)$|\\\"", "",
        grep ("^exportPattern", nmsp, value = TRUE)
    )
    fns_exp_ptn <- grep (exp_ptn, tags_r$tag, value = TRUE)

    return (fns_exp_ptn)
}

#' Get all aliases from .Rd files
#'
#' These are matched to actual function names from
#' `exp_ptn_r_fn_names()`, and any class, data, method, or package definitions
#' are removed.
#'
#' Removal of 'class', 'data', and 'package' aliases is straightforward. Removal
#' of 'method' aliases simply ignores any aliases within any .Rd file that also
#' includes a 'method' section. That is not entirely reliable, and may also
#' include genuine function names.
#' @noRd
aliases_from_rd <- function (path, nmsp) {

    tarball <- grepl ("\\.tar\\.gz$", path)

    if (tarball) {

        flist <- utils::untar (
            path,
            exdir = fs::path_temp (),
            list = TRUE,
            tar = "internal"
        )

        rd <- grep ("\\.Rd$", flist, value = TRUE)

        chk <- utils::untar (
            path,
            files = rd,
            exdir = fs::path_temp ()
        )
        if (chk != 0) {
            return (NULL)
        }

        flist <- fs::path (fs::path_temp (), rd)

    } else {

        flist <- fs::dir_ls (
            path,
            regexp = "\\.Rd$",
            recurse = TRUE
        )
    }

    flist <- expand_path (flist)
    # only extract Rd files from man directory:
    is_man <- vapply (
        fs::path_split (flist),
        function (i) {
            any (i == "man")
        },
        logical (1L)
    )
    flist <- flist [which (is_man)]

    nms <- lapply (flist, function (i) {

        rd_i <- tools::parse_Rd (i)

        doc_type <- get_Rd_metadata (rd_i, "docType")
        doc_type <- ifelse (length (doc_type) == 0L, "", doc_type)

        out <- NULL

        if (!doc_type %in% c ("class", "data", "methods", "package")) {
            out <- unique (c (
                get_Rd_metadata (rd_i, "alias")
            ))
            if (length (out) == 0L) {
                get_Rd_metadata (rd_i, "name")
            }

            index <- grep ("method(s?|,?)$|class$|<\\-|\\[\\[|,|\\s", out)
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

        return (out)
    })

    nms <- unique (unlist (nms))

    nms_exp_ptn <- exp_ptn_r_fn_names (path, nmsp)

    return (nms [which (nms %in% nms_exp_ptn)])
}

get_pkg_name_version <- function (desc) {

    d <- data.frame (read.dcf (desc))

    return (c (d$Package, d$Version))
}

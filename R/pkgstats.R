#' Analyse statistics of one R package
#'
#' @param path Either a path to a local source repository, or a local '.tar.gz'
#' file, containing code for an R package.
#' @return List of statistics and data on function call networks (or object
#' relationships in other languages). Includes the following components:
#' \enumerate{
#'   \item{loc: }{Summary of Lines-of-Code in all package directories}
#'   \item{vignettes: }{Numbers of vignettes and "demo" files}
#'   \item{data_stats: }{Statistics of numbers and sizes of package data files}
#'   \item{desc: }{Summary of contents of 'DESCRIPTION' file}
#'   \item{translations: }{List of translations into other (human) languages
#'   (where provides)}
#'   \item{objects: }{A `data.frame` of all functions in R, and all other
#'   objects (functions, classes, structures, global variables, and more) in all
#'   other languages}
#'   \item{network: }{A `data.frame` of object references within and between all
#'   languages; in R these are function calls, but may be more abstract in other
#'   languages.}
#'   \item{external_calls: }{A `data.frame` of all calls make to all functions
#'   from all other R packages, including base and recommended as well as
#'   contributed packages.}
#' }
#'
#' @family stats
#' @export
#' @examples
#' # 'path' can be path to a package tarball:
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' \dontrun{
#' s <- pkgstats (f)
#' }
#' # or to a source directory:
#' path <- extract_tarball (f)
#' \dontrun{
#' s <- pkgstats (path)
#' }
pkgstats <- function (path = ".") {

    path <- check_path (path)

    tarball <- FALSE
    if (grepl ("\\.tar\\.gz$", path)) {
        tarball <- TRUE
        path <- extract_tarball (path)
    }

    s1 <- loc_stats (path)
    has_tabs <- any (s1$ntabs > 0L) # passed to tags_data below

    num_vignettes <- get_num_vignettes (path)
    num_demos <- get_num_demos (path)
    data_stats <- get_data_stats (path)
    s2 <- desc_stats (path)
    s3 <- rd_stats (path)

    fns <- all_functions (path)

    if (nrow (fns) > 0L) {

        fns$exported <- FALSE
        index <- which (fns$fn_name %in% s3$fn_name)
        fns$exported [which (fns$fn_name %in% s3$fn_name)] <- TRUE

        # non-dplyr left_join:
        index2 <- match (fns$fn_name [index], s3$fn_name)
        fns$num_doclines <- fns$param_nchars_mn <-
            fns$param_nchars_md <- NA_integer_
        fns$num_doclines [index] <- s3$num_doclines [index2]
        fns$param_nchars_mn [index] <- s3$param_nchars_mn [index2]
        fns$param_nchars_md [index] <- s3$param_nchars_md [index2]
    }

    # Running 'ctags_test()' on some CRAN machines takes too long (> 10s), so
    # this flag is used to switch off tagging routines on CRAN tests.
    if (Sys.getenv ("PKGSTATS_CRAN_TESTS") == "true") {
        tags <- dummy_tags_data ()
    } else {
        # `s2$package` mucks up local linter
        tags <- tags_data (path, has_tabs, s2 [["package"]])
    }

    translations <- get_translations (path)

    if (tarball) {
        chk <- unlink (path, recursive = TRUE)
    }

    list (
        loc = s1,
        vignettes = c (
            vignettes = num_vignettes,
            demos = num_demos
        ),
        data_stats = data_stats,
        desc = s2,
        translations = translations,
        objects = add_src_to_fn_data (fns, tags$objects),
        network = tags$network,
        external_calls = tags$external_calls
    )
}

#' Get all exported and internal functions
#' @noRd
all_functions <- function (path) {

    if (Sys.getenv ("PKGSTATS_CRAN_TESTS") == "true") {
        return (all_functions_dummy ())
    }

    r_files <- normalizePath (list.files (
        file.path (path, "R"),
        full.names = TRUE,
        pattern = "\\.(r|R|q|s|S)$"
    ))

    if (length (r_files) == 0L) {
        return (all_functions_dummy ())
    }

    eval1 <- function (f) {

        p <- control_parse (file = f)
        if (methods::is (p, "simpleError")) {
            return (NULL)
        }

        # All functions are parsed to length 3, while things like `"_PACKAGE"`
        # statements are parsed, but have parse lengths < 3.
        p <- p [which (vapply (p, length, integer (1)) > 2L)]

        loc <- vapply (
            p, function (i) {
                length (deparse (i))
            },
            integer (1)
        )
        nms <- vapply (
            p, function (i) {
                paste0 (as.list (i) [[2]], collapse = "")
            },
            character (1)
        )
        npars <- vapply (
            p, function (i) {
                call_i <- as.list (i) [[3]]
                if (length (call_i) < 2) { # not a fn
                    return (rep (NA_integer_, 2))
                }
                c2 <- call_i [[2]]
                c (length (c2), "..." %in% names (c2)) },
            integer (2)
        )

        data.frame (
            file_name = rep (basename (f), length (p)),
            fn_name = nms,
            loc = loc,
            npars = npars [1, ],
            has_dots = as.logical (npars [2, ]),
            stringsAsFactors = FALSE
        )
    }

    ret <- do.call (rbind, lapply (r_files, eval1))
    if (nrow (ret) > 0L) {
        # append "R" directory to file names:
        ret$file_name <- paste0 (
            "R",
            .Platform$file.sep,
            ret$file_name
        )
    }

    return (ret)
}

#' Dummy return from `all_funcitons()` function, triggered only on CRAN tests
#' which otherwise take too long because of the `parse` calls.
#' @noRd
all_functions_dummy <- function () {

    data.frame (
        "file_name" = character (0),
        "fn_name" = character (0),
        "loc" = integer (0),
        "npars" = integer (0),
        "has_dots" = logical (0),
        "exported" = logical (0),
        "param_nchars_md" = numeric (0),
        "param_nchars_mn" = numeric (0),
        "num_doclines" = integer (0)
    )
}

#' param src value of tags$stats from tags_data()
#' @noRd
add_src_to_fn_data <- function (fns, src) {

    if (nrow (fns) == 0L) {
        return (fns)
    }

    n <- grep ("fn_name", names (fns))

    if (length (n) == 0) { # data-only packages

        fns <- data.frame (
            file_name = NA_character_,
            fn_name = NA_character_,
            kind = NA_character_,
            language = NA_character_,
            loc = 0L,
            npars = NA_integer_,
            has_dots = FALSE,
            exported = FALSE,
            param_nchars_md = NA_integer_,
            param_nchars_mn = NA_integer_,
            num_doclines = NA_integer_,
            stringsAsFactors = FALSE
        )
    } else {

        fns <- data.frame (
            fns [, seq (n)], # file_name, fn_name
            kind = "function",
            language = "R",
            fns [, seq (ncol (fns)) [-seq (n)]],
            stringsAsFactors = FALSE
        )
    }

    if (all (is.na (src$file_name))) {

        src <- NULL

    } else {

        src$exported <- FALSE
        nms <- names (fns) [which (!names (fns) %in% names (src))]
        for (n in nms) {
            src [n] <- NA_integer_
        }
        src <- src [, match (names (fns), names (src))]
    }

    out <- rbind (fns, src)

    rownames (out) <- NULL

    return (out)
}

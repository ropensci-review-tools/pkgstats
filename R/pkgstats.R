#' Collates statistics from one local tarball
#'
#' @param path Either a path to a local source repository, or a local '.tar.gz'
#' file containing code for an R package.
#' @return List of statistics
#' @export
#' @examples
#' \dontrun{
#' tarball <- "magrittr_2.0.1.tar.gz"
#' u <- paste0 ("https://cran.r-project.org/src/contrib/",
#'              tarball)
#' f <- file.path (tempdir (), tarball)
#' download.file (u, f)
#' pkgstats (f)
#' }
pkgstats <- function (path) {

    tarball <- FALSE
    if (grepl ("\\.tar\\.gz$", path)) {
        tarball <- TRUE
        path <- extract_tarball (path)
    }

    s1 <- cloc_stats (path)
    num_vignettes <- get_num_vignettes (path)
    s2 <- desc_stats (path)
    s3 <- rd_stats (path)

    fns <- all_functions (path)
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

    tags <- tags_data (path)

    if (tarball)
        chk <- unlink (path, recursive = TRUE)

    list (cloc = s1,
          num_vignettes = num_vignettes,
          desc = s2,
          code_has_tabs = tags$has_tabs,
          objects = add_src_to_fn_data (fns, tags$stats),
          network = tags$network)
}

#' Get all exported and internal functions
#' @noRd
all_functions <- function (path) {

    r_files <- normalizePath (list.files (file.path (path, "R"),
                                          full.names = TRUE))

    eval1 <- function (f) {

        p <- tryCatch (parse (file = f),
                       error = function (err) "error")
        if ("error" %in% p)
            return (NULL)

        # All functions are parsed to length 3, while things like `"_PACKAGE"`
        # statements are parsed, but have parse lengths < 3.
        p <- p [which (vapply (p, length, integer (1)) > 2L)]

        loc <- vapply (p, function (i)
                       length (deparse (i)),
                       integer (1))
        nms <- vapply (p, function (i)
                       paste0 (as.list (i) [[2]], collapse = ""),
                       character (1))
        npars <- vapply (p, function (i) {
                             call_i <- as.list (i) [[3]]
                             if (length (call_i) < 2) # not a fn
                                 return (rep (NA_integer_, 2))
                             c2 <- call_i [[2]]
                             c (length (c2), "..." %in% names (c2)) },
                             integer (2))

        data.frame (file_name = rep (basename (f), length (p)),
                    fn_name = nms,
                    loc = loc,
                    npars = npars [1, ],
                    has_dots = as.logical (npars [2, ]))
    }

    do.call (rbind, lapply (r_files, eval1))
}

#' param src value of tags$stats from tags_data()
#' @noRd
add_src_to_fn_data <- function (fns, src) {

    n <- grep ("fn_name", names (fns))
    fns <- data.frame (fns [, seq (n)],
                       kind = "function",
                       language = "R",
                       fns [, seq (ncol (fns)) [-seq (n)]])

    src$exported <- FALSE
    nms <- names (fns) [which (!names (fns) %in% names (src))]
    for (n in nms)
        src [n] <- NA_integer_

    return (rbind (fns, src))
}

get_num_vignettes <- function (path) {

    nv <- 0

    if ("build" %in% list.files (path)) {

        flist <- list.files (file.path (path, "build"),
                             full.names = TRUE)
        vfile <- grep ("vignette", flist, value = TRUE)
        if (length (vfile) > 0) {
            nv <- nrow (readRDS (vfile [1]))
        }
    } else if ("vignettes" %in% list.files (path)) {
        nv <- length (list.files (file.path (path, "vignettes"),
                                  pattern = "\\.Rmd$|\\.Rnw"))
    }

    return (nv)
}

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
    num_demos <- get_num_demos (path)
    data_stats <- get_data_stats (path)
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
          vignettes = c (vignettes = num_vignettes,
                         demos = num_demos),
          data_stats = data_stats,
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

#' Condense the output of `pkgstats` to summary statistics only
#'
#' @param s Output of \link{pkgstats}, containing full statistical data on one
#' package.
#' @return Summarised version of `s`, as a single row of a standardised
#' `data.frame` object
#' @export
pkgstats_summary <- function (s) {

    out <- data.frame (package = s$desc$package,
                       version = s$desc$version,
                       date = s$desc$date,
                       license = s$desc$license)

    out <- cbind (out, cloc_summary (s$cloc))

    out$num_vignettes <- s$vignettes [1]
    out$num_demos <- s$vignettes [2]
    out$num_data_files <- s$data_stats [1]
    out$data_size_total <- s$data_stats [2]
    out$data_size_median <- s$data_stats [3]

    out <- cbind (out, desc_summary (s$desc))

    out$code_has_tabs <- s$code_has_tabs

    return (out)
}

#' @param x the 'cloc` component of `pkgstats` output.
#' @noRd
cloc_summary <- function (x) {

    data.frame (files_R = x$file_count [x$source == "R"],
                files_src = x$file_count [x$source == "src"],
                files_inst = x$file_count [x$source == "include"],
                files_vignettes = x$file_count [x$source == "vignettes"],
                loc_R = x$loc [x$source == "R"],
                loc_src = x$loc [x$source == "src"],
                loc_inst = x$loc [x$source == "include"],
                loc_vignettes = x$loc [x$source == "vignettes"],
                blank_lines_R = x$blank_lines [x$source == "R"],
                blank_lines_src = x$blank_lines [x$source == "src"],
                blank_lines_inst = x$blank_lines [x$source == "include"],
                blank_lines_vignettes = x$blank_lines [x$source == "vignettes"],
                comment_lines_R = x$comment_lines [x$source == "R"],
                comment_lines_src = x$comment_lines [x$source == "src"],
                comment_lines_inst = x$comment_lines [x$source == "include"],
                comment_lines_vignettes = x$comment_lines [x$source == "vignettes"])
}

#' @param x the 'desc' components of 'pkgstats' output
#' @noRd
desc_summary <- function (x) {

    data.frame (
                urls = s$desc$urls,
                bugs = s$desc$bugs,

                desc_n_aut = s$desc$aut,
                desc_n_ctb = s$desc$ctb,
                desc_n_fnd = s$desc$fnd,
                desc_n_rev = s$desc$rev,
                desc_n_ths = s$desc$ths,
                desc_n_trl = s$desc$trl,

                depends = s$desc$depends,
                imports = s$desc$imports,
                suggests = s$desc$suggests,
                linking_to = s$desc$linking_to)
}

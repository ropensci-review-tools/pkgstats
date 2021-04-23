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
    out$translations <- paste0 (s$translations, collapse = ", ")

    out <- cbind (out, desc_summary (s$desc))

    out$code_has_tabs <- s$code_has_tabs

    out <- cbind (out, object_summary (s$objects))

    out <- cbind (out, network_summary (s$network))

    return (out)
}

#' @param x the 'cloc` component of `pkgstats` output.
#' @noRd
cloc_summary <- function (x) {

    blank <- x [1, ]

    col_nms <- c ("file_count",
                  "file_count_pct",
                  "loc",
                  "loc_pct",
                  "blank_lines",
                  "blank_line_pct",
                  "comment_lines",
                  "comment_line_pct")
    blank [col_nms] <- 0L

    add_if_missing <- function (x, type = "src") {

        if (!type %in% x$source) {

            src <- blank
            src$source <- type
            src$language <- "-"
            x <- rbind (x, src)
        }

        return (x)
    }

    # R shouldn't ever be missing, but maybe? (BDR's boot pkg comes close)
    x <- add_if_missing (x, "R")
    x <- add_if_missing (x, "src")
    x <- add_if_missing (x, "include")
    x <- add_if_missing (x, "vignettes")
    x <- add_if_missing (x, "tests")

    # suppress no visible binding notes
    file_count <- loc <- blank_lines <- comment_lines <- NULL
    # no pipes here
    xg <- dplyr::group_by (x, source)
    x <- dplyr::summarise (xg,
                           file_count = sum (file_count),
                           loc = sum (loc),
                           blank_lines = sum (blank_lines),
                           comment_lines = sum (comment_lines))

    data.frame (files_R = x$file_count [x$source == "R"],
                files_src = x$file_count [x$source == "src"],
                files_inst = x$file_count [x$source == "include"],
                files_vignettes = x$file_count [x$source == "vignettes"],
                files_tests = x$file_count [x$source == "tests"],
                loc_R = x$loc [x$source == "R"],
                loc_src = x$loc [x$source == "src"],
                loc_inst = x$loc [x$source == "include"],
                loc_vignettes = x$loc [x$source == "vignettes"],
                loc_tests = x$loc [x$source == "tests"],
                blank_lines_R = x$blank_lines [x$source == "R"],
                blank_lines_src = x$blank_lines [x$source == "src"],
                blank_lines_inst = x$blank_lines [x$source == "include"],
                blank_lines_vignettes = x$blank_lines [x$source == "vignettes"],
                blank_lines_tests = x$blank_lines [x$source == "tests"],
                comment_lines_R = x$comment_lines [x$source == "R"],
                comment_lines_src = x$comment_lines [x$source == "src"],
                comment_lines_inst = x$comment_lines [x$source == "include"],
                comment_lines_vignettes = x$comment_lines [x$source ==
                                                           "vignettes"],
                comment_lines_tests = x$comment_lines [x$source == "tests"])
}

#' @param x the 'desc' components of 'pkgstats' output
#' @noRd
desc_summary <- function (x) {

    data.frame (
                urls = x$urls,
                bugs = x$bugs,

                desc_n_aut = x$aut,
                desc_n_ctb = x$ctb,
                desc_n_fnd = x$fnd,
                desc_n_rev = x$rev,
                desc_n_ths = x$ths,
                desc_n_trl = x$trl,

                depends = x$depends,
                imports = x$imports,
                suggests = x$suggests,
                linking_to = x$linking_to)
}

#' @param x the 'objects' components of 'pkgstats' output
#' @noRd
object_summary <- function (x) {

    fns <- x [x$kind == "function", ]
    fns$num_doclines [is.na (fns$num_doclines)] <- 0L
    fns_r <- fns [fns$language == "R", ]
    fns_not_r <- fns [fns$language != "R", ]

    n_fns_r <- nrow (fns_r)
    n_fns_r_exported <- length (which (fns_r$exported))
    n_fns_r_not_exported <- n_fns_r - n_fns_r_exported

    n_fns_per_file_r <- nrow (fns_r) / length (unique (fns_r$file_name))
    n_fns_per_file_src <- nrow (fns_not_r) /
        length (unique (fns_not_r$file_name))

    index_exp <- which (fns_r$exported)
    index_not_exp <- which (!fns_r$exported)

    npars_exported_mn <- mean (fns_r$npars [index_exp], na.rm = TRUE)
    npars_exported_md <- stats::median (fns_r$npars [index_exp], na.rm = TRUE)
    loc_per_fn_r_mn <- mean (fns_r$loc, na.rm = TRUE)
    loc_per_fn_r_md <- stats::median (fns_r$loc, na.rm = TRUE)
    loc_per_fn_r_exp_mn <- mean (fns_r$loc [index_exp], na.rm = TRUE)
    loc_per_fn_r_exp_md <- stats::median (fns_r$loc [index_exp], na.rm = TRUE)
    loc_per_fn_r_not_exp_mn <- mean (fns_r$loc [index_not_exp], na.rm = TRUE)
    loc_per_fn_r_not_exp_md <- stats::median (fns_r$loc [index_not_exp],
                                              na.rm = TRUE)

    doclines_per_fn_exp_mn <- mean (fns_r$num_doclines [index_exp],
                                    na.rm = TRUE)
    doclines_per_fn_exp_md <- stats::median (fns_r$num_doclines [index_exp],
                                             na.rm = TRUE)
    doclines_per_fn_not_exp_mn <- mean (fns_r$num_doclines [index_not_exp],
                                        na.rm = TRUE)
    doclines_per_fn_not_exp_md <-
        stats::median (fns_r$num_doclines [index_not_exp], na.rm = TRUE)

    # exract nchars-per-param for exported only:
    nchars_tot <- x$param_nchars_mn * x$npars
    docchars_per_par_exp_mn <- sum (nchars_tot [index_exp], na.rm = TRUE) /
        sum (x$npars [index_exp], na.rm = TRUE)
    nchar <- x$param_nchars_mn [index_exp]
    npar <- x$npars [index_exp]
    index <- which (!is.na (nchar) & !is.na (npar))
    nchars_tot <- rep (nchar [index], times = npar [index])
    docchars_per_par_exp_md <- stats::median (nchars_tot, na.rm = TRUE)

    n_fns_src <- nrow (fns_not_r)
    loc_per_fn_src_mn <- mean (fns_not_r$loc, na.rm = TRUE)
    loc_per_fn_src_md <- stats::median (fns_not_r$loc, na.rm = TRUE)
    languages <- paste0 (unique (fns_not_r$language), collapse = ", ")

    data.frame (n_fns_r = n_fns_r,
                n_fns_r_exported = n_fns_r_exported,
                n_fns_r_not_exported = n_fns_r_not_exported,
                n_fns_src = n_fns_src,
                n_fns_per_file_r = n_fns_per_file_r,
                n_fns_per_file_src = n_fns_per_file_src,
                npars_exported_mn = npars_exported_mn,
                npars_exported_md = npars_exported_md,
                loc_per_fn_r_mn = loc_per_fn_r_mn,
                loc_per_fn_r_md = loc_per_fn_r_md,
                loc_per_fn_r_exp_mn = loc_per_fn_r_exp_mn,
                loc_per_fn_r_exp_md = loc_per_fn_r_exp_md,
                loc_per_fn_r_not_exp_mn = loc_per_fn_r_not_exp_mn,
                loc_per_fn_r_not_exp_md = loc_per_fn_r_not_exp_md,
                loc_per_fn_src_mn = loc_per_fn_src_mn,
                loc_per_fn_src_md = loc_per_fn_src_md,
                languages = languages,
                doclines_per_fn_exp_mn = doclines_per_fn_exp_mn,
                doclines_per_fn_exp_md = doclines_per_fn_exp_md,
                doclines_per_fn_not_exp_mn = doclines_per_fn_not_exp_mn,
                doclines_per_fn_not_exp_md = doclines_per_fn_not_exp_md,
                docchars_per_par_exp_mn = docchars_per_par_exp_mn,
                docchars_per_par_exp_md = docchars_per_par_exp_md)

}

#' @param x the 'network' components of 'pkgstats' output
#' @noRd
network_summary <- function (x) {

    n_clusters <- length (unique (x$cluster_dir))
    n_edges <- ifelse (is.null (x), 0L, nrow (x))

    dirs <- vapply (x$file, function (i)
                    strsplit (i, .Platform$file.sep) [[1]] [1],
                    character (1),
                    USE.NAMES = FALSE)
    n_edges_r <- length (which (dirs == "R"))
    n_edges_src <- length (which (dirs != "R"))

    centrality_dir_mn <-
        centrality_dir_mn_no0 <-
        centrality_dir_md <-
        centrality_dir_md_no0 <-
        centrality_undir_mn <-
        centrality_undir_mn_no0 <-
        centrality_undir_md <-
        centrality_undir_md_no0 <- NA_integer_

    if (nrow (x) > 0) {

        centrality_dir_mn <- mean (x$centrality_dir, na.rm = TRUE)
        centrality_dir_mn_no0 <- mean (x$centrality_dir [x$centrality_dir > 0],
                                       na.rm = TRUE)
        centrality_dir_md <- stats::median (x$centrality_dir, na.rm = TRUE)
        centrality_dir_md_no0 <-
            stats::median (x$centrality_dir [x$centrality_dir > 0],
                           na.rm = TRUE)

        cu <- x$centrality_undir [x$centrality_undir > 0]
        centrality_undir_mn <- mean (x$centrality_undir, na.rm = TRUE)
        centrality_undir_mn_no0 <- mean (cu, na.rm = TRUE)
        centrality_undir_md <- stats::median (x$centrality_undir, na.rm = TRUE)
        centrality_undir_md_no0 <- stats::median (cu, na.rm = TRUE)
    }

    from <- to <- NULL # suppress no visible binding notes
    node_degree <-
        node_degree_mn <-
        node_degree_md <-
        node_degree_max <- 0L

    if (nrow (x) > 0) {

        node_degree <- dplyr::group_by (x, from)
        node_degree <- dplyr::count (node_degree, to)
        node_degree_mn <- mean (node_degree$n)
        node_degree_md <- stats::median (node_degree$n)
        node_degree_max <- max (node_degree$n)
    }

    data.frame (n_edges = n_edges,
                n_edges_r = n_edges_r,
                n_edges_src = n_edges_src,
                n_clusters = n_clusters,
                centrality_dir_mn = centrality_dir_mn,
                centrality_dir_md = centrality_dir_md,
                centrality_dir_mn_no0 = centrality_dir_mn_no0,
                centrality_dir_md_no0 = centrality_dir_md_no0,
                centrality_undir_mn = centrality_undir_mn,
                centrality_undir_md = centrality_undir_md,
                centrality_undir_mn_no0 = centrality_undir_mn_no0,
                centrality_undir_md_no0 = centrality_undir_md_no0,
                node_degree_mn = node_degree_mn,
                node_degree_md = node_degree_md,
                node_degree_max = node_degree_max)
}

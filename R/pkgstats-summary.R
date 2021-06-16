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

    out <- cbind (out, loc_summary (s$loc))

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

#' @param x the 'loc` component of `pkgstats` output.
#' @noRd
loc_summary <- function (x) {

    # suprress no visible binding notes:
    language <- nfiles <- nlines <- ncode <- 
        ndoc <- nempty <- nspaces <- nchars <-  NULL

    xg <- dplyr::group_by (x, dir)
    x <- dplyr::summarise (xg,
                           nfiles = sum (nfiles),
                           ncode = sum (ncode),
                           ndoc = sum (ndoc),
                           nempty = sum (nempty),
                           nspaces = sum (nspaces),
                           nchars = sum (nchars),
                           indentation = stats::median (indentation))

    blank <- x [1, ]
    col_nms <- names (blank) [which (!names (blank) == "dir")]
    blank [col_nms] <- 0L

    add_if_missing <- function (x, type = "src") {

        if (!type %in% x$dir) {

            src <- blank
            src$dir <- type
            x <- rbind (x, src)
        }

        return (x)
    }

    has_code <- any (x$nfiles > 0) # data-only pkgs may have no code at all

    # R shouldn't ever be missing, but maybe? (BDR's boot pkg comes close)
    if (has_code) {

        x <- add_if_missing (x, "R")
        x <- add_if_missing (x, "src")
        x <- add_if_missing (x, "include")
        x <- add_if_missing (x, "vignettes")
        x <- add_if_missing (x, "tests")
    }

    files_R <- files_src <- files_include <-    # nolint
        files_vignettes <- files_tests <- 0L
    rel_space_R <- rel_space_src <- rel_space_include <- 
        rel_space_vignettes <- rel_space_tests <- NA

    if (has_code) {

        rel_space <- x$nspaces / x$nchars
        rel_space [rel_space == 0 | is.nan (rel_space)] <- NA

        dirs <- c ("R", "src", "include", "vignettes", "tests")
        for (d in dirs) {

            assign (paste0 ("files_", d), x$nfiles [x$dir == d])
            assign (paste0 ("loc_", d), x$ncode [x$dir == d])
            assign (paste0 ("blank_lines_", d), x$nempty [x$dir == d])
            assign (paste0 ("comment_lines_", d), x$ndoc [x$dir == d])

            di <- which (x$dir == d)
            assign (paste0 ("rel_space_", d), rel_space [di])
        }
    }

    index <- which (x$dir %in% c ("R", "src", "include"))

    if (files_R == 0)
        loc_R <- blank_lines_R <- comment_lines_R <- NA_integer_ # nolint
    if (files_src == 0)
        loc_src <- blank_lines_src <- comment_lines_src <- NA_integer_
    if (files_include == 0)
        loc_include <-
            blank_lines_include <-
            comment_lines_include <- NA_integer_
    if (files_vignettes == 0)
        loc_vignettes <-
            blank_lines_vignettes <-
            comment_lines_vignettes <- NA_integer_
    if (files_tests == 0)
        loc_tests <- blank_lines_tests <- comment_lines_tests <- NA_integer_

    indentation <- stats::median (x$indentation [x$indentation > 0])

    data.frame (files_R = files_R,
                files_src = files_src,
                files_inst = files_include,
                files_vignettes = files_vignettes,
                files_tests = files_tests,
                loc_R = loc_R,
                loc_src = loc_src,
                loc_inst = loc_include,
                loc_vignettes = loc_vignettes,
                loc_tests = loc_tests,
                blank_lines_R = blank_lines_R,
                blank_lines_src = blank_lines_src,
                blank_lines_inst = blank_lines_include,
                blank_lines_vignettes = blank_lines_vignettes,
                blank_lines_tests = blank_lines_tests,
                comment_lines_R = comment_lines_R,
                comment_lines_src = comment_lines_src,
                comment_lines_inst = comment_lines_include,
                comment_lines_vignettes = comment_lines_vignettes,
                comment_lines_tests = comment_lines_tests,
                rel_space_R = rel_space_R,
                rel_space_src = rel_space_src,
                rel_space_inst = rel_space_include,
                rel_space_vignettes = rel_space_vignettes,
                rel_space_tests = rel_space_tests,
                indentation = indentation)
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
    num_terminal_edges_dir <-
        num_terminal_edges_undir <- NA_integer_

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

        num_terminal_edges_dir <- length (which (x$centrality_dir == 0))
        num_terminal_edges_undir <- length (which (x$centrality_undir == 0))
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
                num_terminal_edges_dir = num_terminal_edges_dir,
                num_terminal_edges_undir = num_terminal_edges_undir,
                node_degree_mn = node_degree_mn,
                node_degree_md = node_degree_md,
                node_degree_max = node_degree_max)
}

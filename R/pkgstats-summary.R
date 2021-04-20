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

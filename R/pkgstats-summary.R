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
}

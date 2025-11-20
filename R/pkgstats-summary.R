#' Condense the output of `pkgstats` to summary statistics only
#'
#' @param s Output of \link{pkgstats}, containing full statistical data on one
#' package. Default of `NULL` returns a single row with `NA` values (used in
#' \link{pkgstats_from_archive}).
#' @return Summarised version of `s`, as a single row of a standardised
#' `data.frame` object
#'
#' @note Variable names in the summary object use the following abbreviations:
#' \itemize{
#'   \item "loc" = Lines-of-Code
#'   \item "fn" = Function
#'   \item "n_fns" = Number of functions
#'   \item "npars" = Number of parameters
#'   \item "doclines" = Number of documentation lines
#'   \item "nedges" = Number of edges in function call network, as a count of
#'   \emph{unique} edges, which may be less than the size of the `network`
#'   object returned by \link{pkgstats}, because that may include multiple calls
#'   between identical function pairs.
#'   \item "n_clusters" = Number of connected clusters within the function call
#'   network.
#'   \item "centrality" used as a prefix for several statistics, along with
#'   "dir" or "undir" for centrality calculated on networks respectively
#'   constructed with directed or undirected edges; "mn" or "md" for respective
#'   measures of mean or median centrality, and "no0" for measures excluding
#'   edges with zero centrality.
#' }
#' @family stats
#' @export
#' @examples
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' @examplesIf ctags_test (noerror = TRUE)
#' p <- pkgstats (f)
#' s <- pkgstats_summary (p)
pkgstats_summary <- function (s = NULL) {

    if (is.null (s)) {
        return (null_stats ())
    }

    out <- data.frame (
        package = s$desc$package,
        version = s$desc$version,
        date = s$desc$date,
        license = s$desc$license,
        stringsAsFactors = FALSE
    )

    out <- cbind (out, loc_summary (s$loc))

    out$num_vignettes <- s$vignettes [1]
    out$num_demos <- s$vignettes [2]
    out$num_data_files <- s$data_stats [1]
    out$data_size_total <- s$data_stats [2]
    out$data_size_median <- s$data_stats [3]
    out$translations <- paste0 (s$translations, collapse = ", ")

    out <- cbind (out, desc_summary (s$desc))

    out <- cbind (out, object_summary (s$objects))

    # Append any languages not parsed by ctags:
    loc_langs <- unique (s$loc$language [s$loc$dir %in% c ("src", "inst")])
    loc_langs <- grep ("^[^R]|^html", loc_langs,
        value = TRUE, ignore.case = TRUE
    )
    langs <- unique (unlist (c (strsplit (out$languages, ",\\s"), loc_langs)))
    out$languages <- paste0 (langs, collapse = ", ")

    out <- cbind (out, network_summary (s$network))

    out <- cbind (out,
        external_calls = external_call_summary (s$external_calls)
    )

    # suffix `_pkg` because inter-package stats are calculated after initial
    # collation of all data (issue #32)
    out$cpl_instability_pkg <- coupling_instability (s)

    return (out)
}

null_stats <- function () {

    out <- data.frame (
        package = NA_character_,
        version = NA_character_,
        date = NA_character_,
        license = NA_character_,
        stringsAsFactors = FALSE
    )

    loc_nms <- c (
        "files_R", "files_src", "files_inst", "files_vignettes",
        "files_tests", "loc_R", "loc_src", "loc_inst",
        "loc_vignettes", "loc_tests", "blank_lines_R",
        "blank_lines_src", "blank_lines_inst",
        "blank_lines_vignettes", "blank_lines_tests",
        "comment_lines_R", "comment_lines_src", "comment_lines_inst",
        "comment_lines_vignettes", "comment_lines_tests", "rel_space",
        "rel_space_R", "rel_space_src", "rel_space_inst",
        "rel_space_vignettes", "rel_space_tests", "indentation",
        "nexpr"
    )
    out [loc_nms] <- NA_integer_

    out$num_vignettes <- NA_integer_
    out$num_demos <- NA_integer_
    out$num_data_files <- NA_integer_
    out$data_size_total <- NA_integer_
    out$data_size_median <- NA_integer_
    out$translations <- NA_character_

    desc_nms <- c (
        "urls", "bugs", "desc_n_aut", "desc_n_ctb", "desc_n_fnd",
        "desc_n_rev", "desc_n_ths", "desc_n_trl", "depends",
        "imports", "suggests", "enhances", "linking_to"
    )
    out [desc_nms] <- NA_character_
    out [grep ("^desc\\_", desc_nms, value = TRUE)] <- NA_integer_

    obj_nms <- c (
        "n_fns_r", "n_fns_r_exported", "n_fns_r_not_exported",
        "n_fns_src", "n_fns_per_file_r", "n_fns_per_file_src",
        "npars_exported_mn", "npars_exported_md", "loc_per_fn_r_mn",
        "loc_per_fn_r_md", "loc_per_fn_r_exp_mn",
        "loc_per_fn_r_exp_md", "loc_per_fn_r_not_exp_mn",
        "loc_per_fn_r_not_exp_md", "loc_per_fn_src_mn",
        "loc_per_fn_src_md", "languages",
        "doclines_per_fn_exp_mn", "doclines_per_fn_exp_md",
        "doclines_per_fn_not_exp_mn", "doclines_per_fn_not_exp_md",
        "doclines_per_fn_src_mn", "doclines_per_fn_src_md",
        "docchars_per_par_exp_mn", "docchars_per_par_exp_md"
    )
    out [obj_nms] <- NA_integer_

    net_nms <- c (
        "n_edges", "n_edges_r", "n_edges_src", "n_clusters",
        "centrality_dir_mn", "centrality_dir_md",
        "centrality_dir_mn_no0", "centrality_dir_md_no0",
        "centrality_undir_mn", "centrality_undir_md",
        "centrality_undir_mn_no0", "centrality_undir_md_no0",
        "num_terminal_edges_dir", "num_terminal_edges_undir",
        "node_degree_mn", "node_degree_md", "node_degree_max"
    )
    out [net_nms] <- NA_integer_

    out$external_calls <- NA_character_
    out$cpl_instability_pkg <- NA

    return (out)
}

#' @param x the 'loc` component of `pkgstats` output.
#' @noRd
loc_summary <- function (x) {

    # suprress no visible binding notes:
    nfiles <- ncode <- ndoc <- nempty <- nspaces <- nchars <- language <- NULL

    indentation <- loc_indentation (x)

    rm_dirs <- c ("inst/doc", "inst/extdata")
    x <- x [which (!x$dir %in% rm_dirs), ]
    rm_langs <- c ("HTML", "CSS", "YAML")
    x <- x [which (!x$language %in% rm_langs), ]

    test_dirs <- c ("inst/tinytest", "tests/testthat")
    x$dir [x$dir %in% test_dirs] <- "tests"

    # Presume everything else in inst is genuine code:
    x$dir [grep ("^inst\\/", x$dir)] <- "inst"

    if ("YAML" %in% x$language) {
        x <- dplyr::filter (x, language != "YAML")
    }
    xg <- dplyr::group_by (x, dir)
    x <- dplyr::summarise (
        xg,
        nfiles = sum (nfiles, na.rm = TRUE),
        ncode = sum (ncode, na.rm = TRUE),
        ndoc = sum (ndoc, na.rm = TRUE),
        nempty = sum (nempty, na.rm = TRUE),
        nspaces = sum (nspaces, na.rm = TRUE),
        nchars = sum (nchars, na.rm = TRUE),
        nexpr = stats::median (nexpr, na.rm = TRUE)
    )
    # nexpr is not truly accurate, but acceptable here

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
        x <- add_if_missing (x, "inst")
        x <- add_if_missing (x, "vignettes")
        x <- add_if_missing (x, "tests")
    }

    files_R <- files_src <- files_inst <- # nolint
        files_vignettes <- files_tests <- 0L
    rel_space_R <- rel_space_src <- rel_space_inst <- # nolint
        rel_space_vignettes <- rel_space_tests <- NA

    if (has_code) {

        rel_space <- x$nspaces / x$nchars
        rel_space [rel_space == 0 | is.nan (rel_space)] <- NA

        dirs <- c ("R", "src", "inst", "vignettes", "tests")
        for (d in dirs) {

            assign (paste0 ("files_", d), x$nfiles [x$dir == d])
            assign (paste0 ("loc_", d), x$ncode [x$dir == d])
            assign (paste0 ("blank_lines_", d), x$nempty [x$dir == d])
            assign (paste0 ("comment_lines_", d), x$ndoc [x$dir == d])

            di <- which (x$dir == d)
            assign (paste0 ("rel_space_", d), rel_space [di])
        }

    }

    if (files_R == 0) { # nolint
        loc_R <- blank_lines_R <- comment_lines_R <- NA_integer_ # nolint
    } # nolint
    if (files_src == 0) {
        loc_src <- blank_lines_src <- comment_lines_src <- NA_integer_
    }
    if (files_inst == 0) {
        loc_inst <- blank_lines_inst <- comment_lines_inst <- NA_integer_
    }
    if (files_vignettes == 0) {
        loc_vignettes <-
            blank_lines_vignettes <-
            comment_lines_vignettes <- NA_integer_
    }
    if (files_tests == 0) {
        loc_tests <- blank_lines_tests <- comment_lines_tests <- NA_integer_
    }

    nexpr <- x$nexpr [which (x$dir %in% c ("inst", "R", "src") & x$nexpr > 0)]
    nexpr <- stats::median (nexpr)

    data.frame (
        files_R = files_R,
        files_src = files_src,
        files_inst = files_inst,
        files_vignettes = files_vignettes,
        files_tests = files_tests,
        loc_R = loc_R,
        loc_src = loc_src,
        loc_inst = loc_inst,
        loc_vignettes = loc_vignettes,
        loc_tests = loc_tests,
        blank_lines_R = blank_lines_R,
        blank_lines_src = blank_lines_src,
        blank_lines_inst = blank_lines_inst,
        blank_lines_vignettes = blank_lines_vignettes,
        blank_lines_tests = blank_lines_tests,
        comment_lines_R = comment_lines_R,
        comment_lines_src = comment_lines_src,
        comment_lines_inst = comment_lines_inst,
        comment_lines_vignettes = comment_lines_vignettes,
        comment_lines_tests = comment_lines_tests,
        rel_space = sum (x$nspaces) / sum (x$nchars),
        rel_space_R = rel_space_R,
        rel_space_src = rel_space_src,
        rel_space_inst = rel_space_inst,
        rel_space_vignettes = rel_space_vignettes,
        rel_space_tests = rel_space_tests,
        indentation = indentation,
        nexpr = nexpr,
        stringsAsFactors = FALSE
    )
}

#' Median indentation from `loc` object
#'
#' @param x The "loc" item returned from main `pkgstats` fn
#' @param tab_threshold If numbers of initial tab characters in relation to
#' total lines of code is greater than this, then code is considered to be
#' tab-indented.
#' @return A positive integer quantifying median indentation of code, or a value
#' of -1L for tab-indentation.
#'
#' @note Indentation metrics are not truly accurate here, but likely good
#' enough. The `loc` object has single values of median indentation per file,
#' and numbers of files in each directory. These values are converted into
#' repeated sequences to obtain a subsequent approximation of the true median.
#' @noRd
loc_indentation <- function (x, tab_threshold = 0.1) {

    index <- which (x$ncode > 0)
    if (length (index) == 0) { # data-only pkgs with no code
        return (NA_integer_)
    }

    tabs_per_line <- mean (x$ntabs [index] / x$ncode [index], na.rm = TRUE)
    if (tabs_per_line > tab_threshold) {
        return (-1L)
    }

    indents <- rep (x$indentation, times = x$nfiles)
    return (stats::median (indents))
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
        enhances = x$enhances,
        linking_to = x$linking_to,
        stringsAsFactors = FALSE
    )
}

#' @param x the 'objects' components of 'pkgstats' output
#' @noRd
object_summary <- function (x) {

    if (nrow (x) == 0L) {
        return (
            data.frame (
                n_fns_r = 0L,
                n_fns_r_exported = 0L,
                n_fns_r_not_exported = 0L,
                n_fns_src = 0L,
                n_fns_per_file_r = 0L,
                n_fns_per_file_src = 0L,
                npars_exported_mn = 0L,
                npars_exported_md = 0L,
                loc_per_fn_r_mn = 0,
                loc_per_fn_r_md = 0,
                loc_per_fn_r_exp_mn = 0,
                loc_per_fn_r_exp_md = 0,
                loc_per_fn_r_not_exp_mn = 0,
                loc_per_fn_r_not_exp_md = 0,
                loc_per_fn_src_mn = 0,
                loc_per_fn_src_md = 0,
                languages = NA_character_,
                doclines_per_fn_exp_mn = 0,
                doclines_per_fn_exp_md = 0,
                doclines_per_fn_not_exp_mn = 0,
                doclines_per_fn_not_exp_md = 0,
                doclines_per_fn_src_mn = 0,
                doclines_per_fn_src_md = 0,
                docchars_per_par_exp_mn = 0,
                docchars_per_par_exp_md = 0,
                stringsAsFactors = FALSE
            )
        )
    }

    fns <- x [x$kind == "function" &
        !grepl ("anonFunc", x$fn_name), ]
    fns$num_doclines [is.na (fns$num_doclines)] <- 0L
    fns_r <- fns [fns$language == "R", ]
    fns_not_r <- fns [fns$language != "R", ]

    n_fns_r <- length (which (!is.na (fns_r$fn_name)))
    n_fns_r_exported <- length (which (!is.na (fns_r$exported) &
        fns_r$exported))
    n_fns_r_not_exported <- n_fns_r - n_fns_r_exported
    repl0withNA <- function (x) { # nolint
        if (x == 0L) {
            x <- NA_integer_
        }
        return (x)
    }
    n_fns_r <- repl0withNA (n_fns_r)
    n_fns_r_exported <- repl0withNA (n_fns_r_exported)
    if (is.na (n_fns_r_exported)) {
        n_fns_r_not_exported <- repl0withNA (n_fns_r_not_exported)
    }
    # otherwise pkgs with all R fns exported should retain 0L here

    get_n_fns_per_file <- function (f) {
        index <- which (!is.na (f$file_name))
        n_files <- length (unique (f$file_name [index]))
        n_fns <- length (unique (f$fn_name [index]))
        ifelse (n_files == 0L, NA_integer_, n_fns / n_files)
    }
    n_fns_per_file_r <- get_n_fns_per_file (fns_r)
    n_fns_per_file_src <- get_n_fns_per_file (fns_not_r)

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
        na.rm = TRUE
    )

    doclines_per_fn_exp_mn <- mean (fns_r$num_doclines [index_exp],
        na.rm = TRUE
    )
    doclines_per_fn_exp_md <- stats::median (fns_r$num_doclines [index_exp],
        na.rm = TRUE
    )
    doclines_per_fn_not_exp_mn <- mean (fns_r$num_doclines [index_not_exp],
        na.rm = TRUE
    )
    doclines_per_fn_not_exp_md <-
        stats::median (fns_r$num_doclines [index_not_exp], na.rm = TRUE)

    doclines_src <- x$num_doclines [grep ("^(src|inst)", x$file_name)]
    doclines_per_fn_src_mn <- mean (doclines_src, na.rm = TRUE)
    doclines_per_fn_src_md <- stats::median (doclines_src, na.rm = TRUE)

    # exract nchars-per-param for exported only:
    nchars_tot <- x$param_nchars_mn * x$npars
    docchars_per_par_exp_mn <- sum (nchars_tot [index_exp], na.rm = TRUE) /
        sum (x$npars [index_exp], na.rm = TRUE)
    nchar <- x$param_nchars_mn [index_exp]
    npar <- x$npars [index_exp]
    index <- which (!is.na (nchar) & !is.na (npar))
    nchars_tot <- rep (nchar [index], times = npar [index])
    docchars_per_par_exp_md <- stats::median (nchars_tot, na.rm = TRUE)

    n_fns_src <- length (which (!is.na (fns_not_r$fn_name)))
    n_fns_src <- repl0withNA (n_fns_src)
    loc_per_fn_src_mn <- mean (fns_not_r$loc, na.rm = TRUE)
    loc_per_fn_src_md <- stats::median (fns_not_r$loc, na.rm = TRUE)
    languages <- paste0 (unique (fns_not_r$language), collapse = ", ")

    data.frame (
        n_fns_r = n_fns_r,
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
        doclines_per_fn_src_mn = doclines_per_fn_src_mn,
        doclines_per_fn_src_md = doclines_per_fn_src_md,
        docchars_per_par_exp_mn = docchars_per_par_exp_mn,
        docchars_per_par_exp_md = docchars_per_par_exp_md,
        stringsAsFactors = FALSE
    )

}

#' @param x the 'network' components of 'pkgstats' output
#' @noRd
network_summary <- function (x) {

    n_clusters <- length (unique (x$cluster_dir))
    n_edges <- ifelse (is.null (x), 0L, nrow (x))

    dirs <- vapply (x$file, function (i) {
        strsplit (i, .Platform$file.sep) [[1]] [1]
    },
    character (1),
    USE.NAMES = FALSE
    )
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
            na.rm = TRUE
        )
        centrality_dir_md <- stats::median (x$centrality_dir, na.rm = TRUE)
        centrality_dir_md_no0 <-
            stats::median (x$centrality_dir [x$centrality_dir > 0],
                na.rm = TRUE
            )

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

    data.frame (
        n_edges = n_edges,
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
        node_degree_max = node_degree_max,
        stringsAsFactors = FALSE
    )
}

#' @param x the 'external_calls' components of 'pkgstats' output
#' @noRd
external_call_summary <- function (x) {

    if (is.null (x)) {
        return (NA_character_)
    }

    # summarise total number of calls to each package, plus number of distinct
    # functions from each:
    package <- NULL # suppress no visible binding note
    n <- dplyr::group_by (x, package, call)
    n <- dplyr::tally (n)
    n <- dplyr::summarise (n,
        ntot = sum (n),
        nunique = length (n)
    )

    res <- apply (n, 1, function (i) {
        paste0 (i, collapse = ":")
    })
    # tibbles put white space in there:
    res <- gsub ("\\s*", "", res)

    res <- data.frame (
        external_calls = paste0 (res, collapse = ","),
        stringsAsFactors = FALSE
    )

    return (res)
}

#' Package-internal metric of coupling instability between files of
#' the package.
#'
#' See 'https://en.wikipedia.org/wiki/Software_package_metrics'
#' Instability within a package is measured here as the proportion of all
#' function calls (or object references) within a package which are made
#' *between* different files compared with total numbers of calls. Packages with
#' relatively more calls confined to within single files are thus more stable,
#' while packages with more calls between different files are less stable.
#' Because a package is a strictly closed system, total numbers of afferent or
#' incoming calls must always equal numbers of efferent or outgoing calls.
#'
#' @inheritParams pkgstats_summary
#' @return Single value for coupling instability
#' @noRd
coupling_instability <- function (s) {

    file_to <- s$objects$file_name [match (s$network$to, s$objects$fn_name)]

    ntot <- nrow (s$network)
    nbetween <- length (which (s$network$file != file_to))

    return (nbetween / ntot)
}

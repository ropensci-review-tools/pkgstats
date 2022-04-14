
#' use ctags and gtags to parse call data
#'
#' @param path Path to local repository
#' @param has_tabs A logical flag indicating whether or not the code contains
#' any tab characters. This can be determined from \link{loc_stats}, which has a
#' `tabs` column. If not given, that value will be extracted from internally
#' calling that function.
#' @param pkg_name Only used for external_call_network, to label
#' package-internal calls.
#' @family tags
#' @examples
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' # have to extract tarball to call function on source code:
#' path <- extract_tarball (f)
#' tags <- tags_data (path)
#' @export
tags_data <- function (path, has_tabs = NULL, pkg_name = NULL) {

    chk <- tryCatch (ctags_test (),
        error = function (e) e
    )
    if (methods::is (chk, "simpleError")) {
        return (dummy_tags_data ())
    }

    if (is.null (has_tabs)) {
        has_tabs <- any (loc_stats (path)$ntabs > 0L)
    } else if (!is.logical (has_tabs) | length (has_tabs) > 1L) {
        stop ("has_tabs must either be NULL or a single logical value")
    }

    if (is.null (pkg_name)) {
        desc <- file.path (path, "DESCRIPTION")
        checkmate::assert_file (desc)
        d <- data.frame (read.dcf (desc), stringsAsFactors = FALSE)
        pkg_name <- d [["Package"]]
    }

    kind <- start <- NULL # no visible binding messages

    tags_r <- withr::with_dir (path, get_ctags ("R", has_tabs))

    external_calls <- external_call_network (tags_r, path, pkg_name)

    tags_src <- withr::with_dir (path, get_ctags ("src", has_tabs))
    tags_inst <- withr::with_dir (path, get_ctags ("inst", has_tabs))

    doclines_src <- count_doclines_src (tags_src, path)
    doclines_inst <- count_doclines_src (tags_inst, path)

    gtags <- NULL

    if (!is.null (tags_src) | !is.null (tags_inst)) {

        no_gtags <- withr::with_dir (path, make_gtags ())

        # gtags does not parse R code, so may return NULL if there is no code in
        # other languages
        gtags <- withr::with_dir (path, get_gtags ())

        ctags <- dplyr::arrange (rbind (tags_src, tags_inst), file, start)
        ctags <- dplyr::filter (ctags, kind %in%
            c ("class", "function", "struct"))

        if (!is.null (gtags)) {

            gtags$from <- NA_character_
            for (f in unique (ctags$file)) {
                gtags <- gtags_from_one_file (ctags, gtags, f)
            }
            gtags <- gtags [which (gtags$tag %in% ctags$tag), ]

            langs <- ctags [, c ("tag", "language")]
            langs <- langs [which (!duplicated (langs)), ]
            gtags$language <- gsub (
                "^language\\:", "",
                langs$language [match (gtags$tag, langs$tag)]
            )
        }

        if (no_gtags) {
            chk <- rm_gtags_files (path)
        }
    }

    fns_r <- tags_r [which (tags_r$kind == "function" & !is.na (tags_r$tag)), ]
    fn_vars_r <- tags_r [which (tags_r$kind == "functionVar" &
        !is.na (tags_r$tag)), ]

    call_graph_r <- fn_var_call_graph_r (
        fns_r,
        fn_vars_r,
        path
    )
    call_graph_src <- fn_var_call_graph_src (gtags)

    network <- rbind (call_graph_r, call_graph_src)
    network <- network [which (!is.na (network$from)), ]

    if (nrow (network) > 0) {

        network <- add_igraph_stats (network, directed = TRUE)
        network <- add_igraph_stats (network, directed = FALSE)
        network$line2 <- NULL
    }

    return (list (
        network = network,
        stats = src_stats (rbind (tags_r, tags_src, tags_inst)),
        external_calls = external_calls,
        doclines = list (
            src = doclines_src,
            inst = doclines_inst
        )
    ))
}

dummy_tags_data <- function () {

    n <- data.frame (matrix (nrow = 0, ncol = 4))
    names (n) <- c ("file", "line1", "from", "to")
    e <- data.frame (matrix (nrow = 0, ncol = 8))
    names (e) <- c (
        "tags_line", "call", "tag", "file", "kind",
        "start", "end", "package"
    )
    return (list (
        network = n,
        external_calls = e
    ))
}

#' Replace tab indentation with a fixed number of spaces
#'
#' This is necessary because the files produced by both ctags and global are
#' unable to be propertly parsed when code contains tab indents. Because this
#' function modifies code, the entire directory, `d`, is copied to `tempdir()`,
#' and the resultant path returned.
#'
#' @param d A directory in which tab indents are to be replaced in all files
#' @param nspaces The equivalent number of spaces with which to replace tab
#' indentations. This parameter has no effect on results.
#' @noRd
rm_tabs <- function (d, nspaces = 2) {

    tmpd <- paste0 (sample (c (letters, LETTERS), size = 8, replace = TRUE),
        collapse = ""
    )
    tmpd <- file.path (tempdir (), tmpd)
    dir.create (tmpd, recursive = TRUE)
    chk <- file.copy (d, tmpd, recursive = TRUE) # copies 'd' as sub-dir of tmpd
    if (any (!chk)) {
        stop ("Unable to copy files from [", d, "] to tempdir()")
    }

    sp <- paste0 (rep (" ", nspaces), collapse = "")

    files <- normalizePath (list.files (
        tmpd,
        full.names = TRUE,
        recursive = TRUE
    ))

    exts <- file_exts ()
    exts$ext <- gsub ("+", "\\+", exts$ext, fixed = TRUE)
    exts$ext <- gsub ("-", "\\-", exts$ext, fixed = TRUE)
    exts <- paste0 (exts$ext, "$", collapse = "|")
    files <- files [grep (exts, files)]
    files <- files [which (!grepl ("^Makevars", files))]

    for (f in files) {
        x <- suppressWarnings (brio::read_lines (f))
        has_tabs <- any (grepl ("\\t", x))
        if (has_tabs) {
            x <- gsub ("^\\t", sp, x)
            x <- gsub ("\\t", " ", x) # replace non-leading tabs with single
            brio::write_lines (x, path = f)
        }
    }

    return (normalizePath (tmpd))
}

#' convert tags_src into same format as the function summary of R code
#' @param tags tags_src from main fn
#' @noRd
src_stats <- function (tags) {

    res <- data.frame (
        file_name = NA_character_,
        fn_name = NA_character_,
        kind = NA_character_,
        language = NA_character_,
        loc = 0L,
        npars = NA_integer_,
        has_dots = NA,
        stringsAsFactors = FALSE
    )

    if (!is.null (tags)) {

        res <- data.frame (
            file_name = tags$file,
            fn_name = tags$tag,
            kind = tags$kind,
            language = gsub ("^language:", "", tags$language),
            loc = tags$end - tags$start + 1,
            npars = NA_integer_,
            has_dots = NA,
            stringsAsFactors = FALSE
        )
        # ctags does not count loc in Rust:
        res <- res [which (!is.na (res$loc) |
            res$language == "Rust"), ]
    }

    return (res)
}

#' Count documentation lines in src files.
#'
#' Currently only implemented for C and C++
#' @param tags_src The output of `get_ctags("src")`.
#' @return An integer vector of length equal to numbers of rows in `tags_src`,
#' counting numbers of documentation lines for each tagged object.
#' @noRd
count_doclines_src <- function (tags_src, path) {

    res <- vapply (seq (nrow (tags_src)), function (i) {

        ndoclines <- 0L

        f <- file.path (path, tags_src$file [i])
        code <- brio::read_lines (f)
        tag_line_start <- tags_src$start [i]
        # ctags does not tag end line numbers of Rust objects:
        if (tags_src$language [i] == "language:Rust") {
            all_ends <- grep ("^\\}", code)
            prev_end <- 1L
            if (any (all_ends < tag_line_start)) {
                prev_end <- max (all_ends [which (all_ends < tag_line_start)])
            }
            not_code <- code [seq (prev_end + 1L, tag_line_start - 1L)]
            ndoclines <- length (grep ("^\\s*\\/\\/", not_code))
        } else {
            all_line_nums <- tags_src [
                tags_src$file == tags_src$file [i],
                c ("start", "end")
            ]
            all_line_nums <- all_line_nums [all_line_nums$end < tag_line_start, ]
            if (nrow (all_line_nums) > 0L) {
                tag_not_code <- seq (
                    max (all_line_nums$end) + 1,
                    tag_line_start - 1
                )
                not_code <- code [tag_not_code]
                if (tags_src$language [i] %in% c ("language:C", "language:C++")) {
                    ndoclines <- length (grep ("^\\/\\/", not_code))
                }
            }
        }

        return (ndoclines)
    }, integer (1L))

    names (res) <- tags_src$tag

    return (res)
}

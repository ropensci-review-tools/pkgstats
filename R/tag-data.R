#' use ctags and gtags to parse call data
#'
#' @param path Path to local repository
#' @param has_tabs A logical flag indicating whether or not the code contains
#' any tab characters. This can be determined from \link{loc_stats}, which has a
#' `tabs` column. If not given, that value will be extracted from internally
#' calling that function.
#' @param pkg_name Only used for external_call_network, to label
#' package-internal calls.
#' @return A list of three items:
#' \itemize{
#' \item "network" A `data.frame` of relationships between objects, generally as
#' calls between functions in R, but other kinds of relationships in other
#' source languages. This is effectively an edge-based network representation,
#' and the data frame also include network metrics for each edge, calculated
#' through representing the network in both directed (suffix "_dir") and
#' undirected (suffix "_undir") forms.
#' \item "objects" A `data.frame` of statistics on each object (generally
#' functions in R, and other kinds of objects in other source languages),
#' including the kind of object, the language, numbers of lines-of-code,
#' parameters, and lines of documentation, and a binary flag indicating whether
#' or not R functions accept "three-dots" parameters (`...`).
#' \item "external_calls" A `data.frame` of every call from within every R
#' function to any external R package, including base and recommended packages.
#' The location of each calls is recorded, along with the external function and
#' package being called.
#' }
#' @family tags
#' @export
#' @examples
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' # have to extract tarball to call function on source code:
#' path <- extract_tarball (f)
#' @examplesIf ctags_test (noerror = TRUE)
#' tags <- tags_data (path)
tags_data <- function (path, has_tabs = NULL, pkg_name = NULL) {

    chk <- tryCatch (ctags_test (noerror = FALSE),
        error = function (e) e
    )
    if (methods::is (chk, "simpleError")) {
        return (dummy_tags_data ())
    }

    if (is.null (has_tabs)) {
        has_tabs <- any (loc_stats (path)$ntabs > 0L)
    } else if (!is.logical (has_tabs) || length (has_tabs) > 1L) {
        stop ("has_tabs must either be NULL or a single logical value")
    }

    if (is.null (pkg_name)) {
        desc <- fs::path (path, "DESCRIPTION")
        checkmate::assert_file (desc)
        d <- data.frame (read.dcf (desc), stringsAsFactors = FALSE)
        pkg_name <- d [["Package"]]
    }

    kind <- start <- NULL # no visible binding messages

    tags_r <- withr::with_dir (path, get_ctags ("R", has_tabs))

    external_calls <- external_call_network (tags_r, path, pkg_name)

    tags_src <- withr::with_dir (path, get_ctags ("src", has_tabs))
    tags_inst <- withr::with_dir (path, get_ctags ("inst", has_tabs))

    tags_src <- count_doclines_src (tags_src, path)
    tags_inst <- count_doclines_src (tags_inst, path)

    gtags <- NULL

    if (!is.null (tags_src) || !is.null (tags_inst)) {

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

    # Functions defined from namespaces of other packages are tagged as global variables.
    kinds <- c ("function", "globalVar")
    fns_r <- tags_r [which (tags_r$kind %in% kinds & !is.na (tags_r$tag)), ]
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

    if (!is.null (tags_r)) {
        tags_r$doclines <- NA_integer_
    }

    return (list (
        network = network,
        objects = src_objects (rbind (tags_r, tags_src, tags_inst)),
        external_calls = external_calls
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
    tmpd <- fs::path (fs::path_temp (), tmpd)
    fs::dir_create (tmpd, recurse = TRUE)
    chk <- fs::dir_copy (d, tmpd) # copies 'd' as sub-dir of tmpd
    if (length (chk) != length (d)) {
        stop ("Unable to copy files from [", d, "] to tempdir()")
    }

    sp <- paste0 (rep (" ", nspaces), collapse = "")

    files <- expand_path (fs::dir_ls (tmpd, recurse = TRUE, type = "file"))

    exts <- file_exts ()
    exts$ext <- gsub ("+", "\\+", exts$ext, fixed = TRUE)
    exts$ext <- gsub ("-", "\\-", exts$ext, fixed = TRUE)
    exts <- paste0 (exts$ext, "$", collapse = "|")
    files <- files [grep (exts, files)]
    files <- files [which (!grepl ("^Makevars", files))]
    # also remove pdf and html files
    index <- which (!grepl ("\\.(pdf|html)$", files, ignore.case = TRUE))
    files <- files [index]

    for (f in files) {
        x <- tryCatch (
            suppressWarnings (brio::read_lines (f)),
            error = function (e) NULL
        )
        if (is.null (x)) {
            next
        }

        has_tabs <- any (grepl ("\\t", x))
        if (has_tabs) {
            # The input string from 'brio' above can be invalid and cause
            # 'gsub' to fail (try on antiword)
            xt <- tryCatch (gsub ("^\\t", sp, x), error = function (e) x)
            # And replace non-leading tabs with single
            xt <- tryCatch (gsub ("\\t", " ", xt), error = function (e) xt)
            if (!identical (x, xt)) {
                brio::write_lines (x, path = f)
            }
        }
    }

    return (expand_path (tmpd))
}

#' convert tags_src into same format as the function summary of R code
#' @param tags tags_src from main fn
#' @noRd
src_objects <- function (tags) {

    res <- data.frame (
        file_name = NA_character_,
        fn_name = NA_character_,
        kind = NA_character_,
        language = NA_character_,
        loc = 0L,
        npars = NA_integer_,
        has_dots = NA,
        num_doclines = NA_integer_,
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
            num_doclines = tags$doclines,
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
#' @param tags The output of `get_ctags("src")` or `get_ctags("inst")`
#' @return Modified version of `tags` with additional `doclines` column
#' counting numbers of documentation lines for each tagged object.
#' @noRd
count_doclines_src <- function (tags, path) {

    if (length (tags) == 0L) {
        return (tags)
    }

    tags$language [which (is.na (tags$language))] <- ""

    res <- vapply (seq_len (nrow (tags)), function (i) {

        ndoclines <- 0L

        f <- fs::path (path, tags$file [i])
        code <- brio::read_lines (f)
        tag_line_start <- tags$start [i]
        # ctags does not tag end line numbers of Rust objects:
        if (tags$language [i] == "language:Rust") {
            all_ends <- grep ("^\\}", code)
            prev_end <- 1L
            if (any (all_ends < tag_line_start)) {
                prev_end <- max (all_ends [which (all_ends < tag_line_start)])
            }
            not_code <- code [seq (prev_end + 1L, tag_line_start - 1L)]
            ndoclines <- length (grep ("^\\s*\\/\\/", not_code))
        } else {
            all_line_nums <- tags [
                tags$file == tags$file [i],
                c ("start", "end")
            ]
            all_line_nums <- all_line_nums [
                all_line_nums$end < tag_line_start,
            ]
            all_line_nums <- all_line_nums [
                which (!is.na (all_line_nums$start) &
                    !is.na (all_line_nums$end)),
            ]
            if (nrow (all_line_nums) > 0L) {
                tag_not_code <- seq (
                    max (all_line_nums$end, na.rm = TRUE) + 1,
                    tag_line_start - 1
                )
                not_code <- code [tag_not_code]

                this_lang <- gsub ("^language\\:", "", tags$language [i])
                exts <- file_exts ()
                ext_i <- exts [match (this_lang, exts$type), ]
                ndoclines <- length (grep (ext_i$cmt, not_code))
                if (ndoclines == 0L & nzchar (ext_i$cmt_open)) {
                    cmt_open <- grep (ext_i$cmt_open, not_code)
                    cmt_close <- grep (ext_i$cmt_close, not_code)
                    if (length (cmt_open) > 0L & length (cmt_close) > 0L) {
                        ndoclines <- max (cmt_close, na.rm = TRUE) -
                            min (cmt_open, na.rm = TRUE)
                    }
                }
            }
        }

        return (ndoclines)
    }, integer (1L))

    tags$doclines <- res

    return (tags)
}

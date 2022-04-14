
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

#' Get tags for one directory within a package
#' @param d the directory
#' @noRd
get_ctags <- function (d = "R", has_tabs) {

    if (!dir.exists (file.path (getwd (), d))) {
        return (NULL)
    }

    path_dir <- file.path (getwd (), d)

    # tab-characters muck up parsing of tag content so have to be removed.
    # This requires modifying the code, so the whole directory is copied to
    # tempdir() and the new path returned. `path_sub` in the following is the
    # path to substitute out of file names given by ctags
    wd <- path_sub <- getwd ()
    if (has_tabs) {
        path_sub <- path_dir <- rm_tabs (path_dir)
        path_dir <- fs::path_tidy (normalizePath (file.path (path_dir, d)))
        wd <- setwd (path_dir)
        on.exit ({
            unlink (path_sub, recursive = TRUE)
            setwd (wd)
        })
        # called via withr::with_path anyway, so doesn't really matter
    }

    # ctags fields defines at
    # https://docs.ctags.io/en/latest/man/ctags.1.html#extension-fields
    # fields:
    #   - e: Line number for end of object
    #   - F: Name of source file
    #   - K: Kind of tag as long name
    #   - z: The kind key in kind field
    #   - l: language
    #   - n: Line number where `name` is defined
    #   - N: Name of language object
    #   - S: Language-specific signature of object
    #   - t: type and name of a variable

    if (d == "R") {
        fields <- "eFKlnN"
    } else if (d %in% c ("src", "inst")) {
        fields <- "eFKlnN"
    }

    ptn <- paste0 ("ctags-", Sys.getpid (), "-")
    f <- tempfile (pattern = ptn, fileext = ".txt")
    args <- c (
        "-R",
        paste0 ("--fields=", fields),
        paste0 ("-f ", f),
        path_dir
    )
    sys::exec_wait ("ctags", args, std_out = FALSE, std_err = FALSE)
    wait_for_process ("ctags")

    # remove header lines:
    x <- brio::read_lines (f)
    x <- x [-which (grepl ("^\\!", x))]

    if (length (x) == 0L) {
        chk <- rm_file_no_err (f)
        return (NULL)
    } # no ctags

    brio::write_lines (x, path = f)

    ctypes <- list (
        readr::col_character (),
        readr::col_character (),
        readr::col_character (),
        readr::col_character (),
        readr::col_character (),
        readr::col_character (),
        readr::col_character ()
    )
    cnames <- c ("tag", "file", "content", "kind", "start", "language", "end")

    n_flds <- readr::count_fields (f,
        tokenizer = readr::tokenizer_tsv (),
        n_max = 100L
    )
    n_flds <- stats::median (n_flds)
    if (n_flds != length (cnames)) {
        chk <- rm_file_no_err (f)
        return (NULL)
    }

    suppressWarnings (
        tags <- readr::read_tsv (
            f,
            col_names = cnames,
            col_types = ctypes,
            col_select = cnames,
            progress = FALSE,
            lazy = FALSE
        )
    )

    chk <- rm_file_no_err (f)

    if (nrow (tags) == 0) {
        return (NULL)
    }

    tags <- tags [which (!grepl (excluded_file_ptn (), tags$file)), ]

    tags$start <- as.integer (gsub ("^line\\:", "", tags$start))

    # end tags may fail, and dump text other than "end:XX", so:
    index0 <- grep ("^end\\:", tags$end)
    index1 <- grep ("^[[:alpha:]]", tags$end)
    index1 <- index1 [which (!index1 %in% index0)]
    tags$end [index1] <- NA

    index <- which (!is.na (tags$end))
    tags$end [index] <- gsub ("^end\\:", "", tags$end [index])
    tags$end [index] <- gsub ("[^0-9.-]", "", tags$end [index])
    # as.integer still triggers warnings for NA values, whereas changing
    # storage.mode does not:
    storage.mode (tags$end) <- "integer"

    files <- fs::path_split (tags$file)
    len_path_sub <- length (fs::path_split (path_sub) [[1]])
    tags$file <- vapply (
        files, function (i) {
            do.call (file.path, as.list (i [-seq (len_path_sub)]))
        },
        character (1)
    )

    attr (tags, "has_tabs") <- has_tabs

    return (tags)
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

fn_var_call_graph_r <- function (fns, fn_vars, path) {

    res <- NULL

    for (f in unique (fns$file)) {

        fns_f <- fns [fns$file == f, ]
        fns_f <- fns_f [order (fns_f$start), c ("tag", "start", "end")]
        fns_index <- lapply (seq (nrow (fns_f)), function (i) {
            cbind (i, seq (fns_f$start [i], fns_f$end [i]))
        })
        fns_index <- do.call (rbind, fns_index)

        f_full <- fs::path_tidy (normalizePath (file.path (path, f)))

        p <- control_parse (file = f_full)
        if (methods::is (p, "simpleError")) {
            next
        }

        pd <- utils::getParseData (control_parse (file = f_full))

        fn_calls <- pd [pd$text %in% fns$tag &
            pd$token == "SYMBOL_FUNCTION_CALL", ]
        index <- match (fn_calls$line1, fns_index [, 2])
        fn_calls$fns_index <- fns_index [index, 1]
        fn_calls$name <- fns_f$tag [fn_calls$fns_index]

        if (nrow (fn_calls) > 0) {

            res <- rbind (
                res,
                data.frame (
                    file = f,
                    line1 = fn_calls$line1,
                    line2 = fn_calls$line2,
                    from = fn_calls$name,
                    to = fn_calls$text,
                    language = "R",
                    stringsAsFactors = FALSE
                )
            )
        }
    }

    return (res)
}

fn_var_call_graph_src <- function (gtags) {

    data.frame (
        file = gtags$file,
        line1 = gtags$line,
        line2 = gtags$line,
        from = gtags$from,
        to = gtags$tag,
        language = gtags$language,
        stringsAsFactors = FALSE
    )
}

add_igraph_stats <- function (g, directed = TRUE) {

    g_igr <- igraph::graph_from_data_frame (g [, c ("from", "to")],
        directed = directed
    )

    cl <- igraph::clusters (g_igr)
    index <- match (g$from, names (cl$membership))
    if (directed) {
        nms <- c ("cluster_dir", "centrality_dir")
    } else {
        nms <- c ("cluster_undir", "centrality_undir")
    }
    g [nms [1]] <- cl$membership [index]
    btw <- igraph::betweenness (g_igr)
    g [nms [2]] <- btw [match (g$from, names (btw))]

    return (g)
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
        res <- res [which (!is.na (res$loc)), ]
    }

    return (res)
}


#' use ctags and gtags to parse call data
#'
#' @param path Path to local repository
#' @param has_tabs A logical flag indicating whether or not the code contains
#' any tab characters. This can be determined from \link{loc_stats}, which has a
#' `tabs` column. If not given, that value will be extracted from internally
#' calling that function.
#' @param pkg_name Only used for external_call_network, to label
#' package-internal calls.
#' @export
tags_data <- function (path, has_tabs = NULL, pkg_name) {

    if (is.null (has_tabs))
        has_tabs <- any (loc_stats (path)$ntabs > 0L)
    else if (!is.logical (has_tabs) | length (has_tabs) > 1L)
        stop ("has_tabs must either be NULL or a single logical value")

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
            for (f in unique (ctags$file))
                gtags <- gtags_from_one_file (ctags, gtags, f)
            gtags <- gtags [which (gtags$tag %in% ctags$tag), ]

            langs <- ctags [, c ("tag", "language")]
            langs <- langs [which (!duplicated (langs)), ]
            gtags$language <- gsub ("^language\\:", "",
                                langs$language [match (gtags$tag, langs$tag)])
        }

        if (no_gtags)
            chk <- rm_gtags_files (path)
    }

    fns_r <- tags_r [tags_r$kind == "function", ]
    fn_vars_r <- tags_r [tags_r$kind == "functionVar", ]

    call_graph_r <- fn_var_call_graph_r (fns_r,
                                         fn_vars_r,
                                         path)
    call_graph_src <- fn_var_call_graph_src (gtags)

    network <- rbind (call_graph_r, call_graph_src)
    network <- network [which (!is.na (network$from)), ]

    if (nrow (network) > 0) {

        network <- add_igraph_stats (network, directed = TRUE)
        network <- add_igraph_stats (network, directed = FALSE)
        network$line2 <- NULL
    }

    return (list (network = network,
                  stats = src_stats (rbind (tags_r, tags_src, tags_inst)),
                  external_calls = external_calls))
}

#' Get tags for one directory within a package
#' @param d the directory
#' @noRd
get_ctags <- function (d = "R", has_tabs) {

    if (!dir.exists (file.path (getwd (), d)))
        return (NULL)

    path_dir <- file.path (getwd (), d)

    # tab-characters muck up parsing of tag content so have to be removed.
    # This requires modifying the code, so the whole directory is copied to
    # tempdir() and the new path returned. `path_sub` in the following is the
    # path to substitute out of file names given by ctags
    wd <- path_sub <- getwd ()
    if (has_tabs) {
        path_sub <- path_dir <- rm_tabs (path_dir)
        path_dir <- normalizePath (file.path (path_dir, d))
        wd <- setwd (path_dir)
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
    args <- c ("-R",
               paste0 ("--fields=",fields),
               paste0 ("-f ", f),
               path_dir)
    sys::exec_wait ("ctags", args)
    Sys.sleep (0.2)

    setwd (wd) # called via withr::with_path anyway, so doesn't really matter

    # remove header lines:
    x <- brio::read_lines (f)
    x <- x [-which (grepl ("^\\!", x))]

    if (length (x) == 0L)
        return (NULL) # no ctags

    writeLines (x, con = f)

    ctypes <- list (readr::col_character (),
                    readr::col_character (),
                    readr::col_character (),
                    readr::col_character (),
                    readr::col_character (),
                    readr::col_character (),
                    readr::col_character ())
    cnames <- c ("tag", "file", "content", "kind", "start", "language", "end")

    n_flds <- readr::count_fields (f,
                                   tokenizer = readr::tokenizer_tsv (),
                                   n_max = 100L)
    n_flds <- stats::median (n_flds)
    if (n_flds != length (cnames))
        return (NULL)


    suppressWarnings (
                      tags <- readr::read_tsv (f,
                                               col_names = cnames,
                                               col_types = ctypes,
                                               col_select = cnames,
                                               progress = FALSE,
                                               lazy = FALSE)
                      )

    if (nrow (tags) == 0)
        return (NULL)

    chk <- tryCatch (file.remove (f),
                     error = function (e) NULL)

    # rm svg files
    tags <- tags [which (!tools::file_ext (tags$file) == "svg"), ]

    tags$start <- as.integer (gsub ("^line\\:", "", tags$start))

    # end tags may fail, and dump text other than "end:XX", so:
    index0 <- grep ("^end\\:", tags$end)
    index1 <- grep ("^[[:alpha:]]", tags$end)
    index1 <- index1 [which (!index1 %in% index0)]
    tags$end [index1] <- NA

    tags$end <- as.integer (gsub ("^end\\:", "", tags$end))

    files <- decompose_path (tags$file)
    len_path_sub <- length (decompose_path (path_sub) [[1]])
    tags$file <- vapply (files, function (i)
                     do.call (file.path, as.list (i [-seq (len_path_sub)])),
                     character (1))

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
                    collapse = "")
    tmpd <- file.path (tempdir (), tmpd)
    dir.create (tmpd, recursive = TRUE)
    chk <- file.copy (d, tmpd, recursive = TRUE) # copies 'd' as sub-dir of tmpd
    if (any (!chk))
        stop ("Unable to copy files from [", d, "] to tempdir()")

    sp <- paste0 (rep (" ", nspaces), collapse = "")

    files <- normalizePath (list.files (tmpd,
                                        full.names = TRUE,
                                        recursive = TRUE))

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
            writeLines (x, con = f)
        }
    }

    return (normalizePath (tmpd))
}

#' Set up gtags files if not already used
#'
#' @return `TRUE` if there are no pre-existing gtags files; otherwise `FALSE`.
#' @noRd
make_gtags <- function () {

    path <- normalizePath (".")
    flist <- list.files (path,
                         recursive = TRUE,
                         full.names = TRUE,
                         pattern = "GRTAGS$|GPATH$|GTAGS$")

    if (length (flist) == 0) {
        sys::exec_wait ("gtags")
        Sys.sleep (0.2)
    }

    return (length (flist) == 0)
}

get_gtags <- function () {

    f <- tempfile (pattern = "global_")
    sys::exec_wait ("global", args = c ("-rx", "."), std_out = f)
    Sys.sleep (0.1)

    x <- brio::read_lines (f)

    # global may fail to parse files, as happens for example with "rms" package
    if (length (x) == 0)
        return (NULL)

    # these are fixed width, but only have 4 cols, so can just replace the first
    # 3 lots of space with single tab characters.
    for (i in 1:3)
        x <- sub (" +", "\t", x)

    ctypes <- list (readr::col_character (),
                    readr::col_double (),
                    readr::col_character (),
                    readr::col_character ())
    cnames <- c ("tag", "line", "file", "content")
    suppressWarnings (
        gtags <- readr::read_tsv (paste0 (x, collapse = "\n"),
                                  col_names = cnames,
                                  col_types = ctypes,
                                  col_select = cnames,
                                  progress = FALSE,
                                  lazy = FALSE)
        )

    # rm header files:
    gtags <- gtags [which (!grepl ("\\.h$", gtags$file)), ]

    return (gtags)
}

#' Put ctag tag objects onto the gtag references where they are called from
#' @param f one '/src' file for for which to assign ctag tags
#' @noRd
gtags_from_one_file <- function (ctags, gtags, f) {

    ctags_f <- ctags [ctags$file == f, ]
    # tags are duplicated for things like class constructors. The duplications
    # are always embedded within the main definition, so simply removing them
    # reduces line ranges to main definition only.
    ctags_f <- ctags_f [which (!duplicated (ctags_f$tag)), ]
    # end lines are not always given, as in Fortran code for which ctags works
    # but gtags does not
    if (any (is.na (ctags_f$end)))
        return (gtags)

    line_nums <- lapply (seq (nrow (ctags_f)), function (i) {
                             data.frame (n = i,
                                         l = seq (ctags_f$start [i],
                                                  ctags_f$end [i]))
                        })
    line_nums <- do.call (rbind, line_nums)

    index <- which (gtags$file == f)
    items <- line_nums$n [match (gtags$line [index], line_nums$l)]
    gtags$from [index] <- ctags_f$tag [items]

    return (gtags)
}

rm_gtags_files <- function (path) {

    flist <- list.files (path,
                         recursive = TRUE,
                         full.names = TRUE,
                         pattern = "GRTAGS$|GPATH$|GTAGS$")

    ret <- NULL

    if (length (flist) > 0) {

        ret <- tryCatch (file.remove (flist),
                         error = function (e) e)
    }

    return (ret)
}

fn_var_call_graph_r <- function (fns, fn_vars, path) {

    res <- NULL

    for (f in unique (fns$file)) {

        fns_f <- fns [fns$file == f, ]
        fns_f <- fns_f [order (fns_f$start), c ("tag", "start", "end")]
        fns_index <- lapply (seq (nrow (fns_f)), function (i)
                             cbind (i, seq (fns_f$start [i], fns_f$end [i])))
        fns_index <- do.call (rbind, fns_index)

        f_full <- normalizePath (file.path (path, f))

        p <- control_parse (file = f_full)
        if (methods::is (p, "simpleError"))
            next

        pd <- utils::getParseData (control_parse (file = f_full))

        fn_calls <- pd [pd$text %in% fns$tag &
                        pd$token == "SYMBOL_FUNCTION_CALL", ]
        index <- match (fn_calls$line1, fns_index [, 2])
        fn_calls$fns_index <- fns_index [index, 1]
        fn_calls$name <- fns_f$tag [fn_calls$fns_index]

        if (nrow (fn_calls) > 0) {

            res <- rbind (res,
                          data.frame (file = f,
                                      line1 = fn_calls$line1,
                                      line2 = fn_calls$line2,
                                      from = fn_calls$name,
                                      to = fn_calls$text,
                                      language = "R"))
        }
    }

    return (res)
}

fn_var_call_graph_src <- function (gtags) {

    data.frame (file = gtags$file,
                line1 = gtags$line,
                line2 = gtags$line,
                from = gtags$from,
                to = gtags$tag,
                language = gtags$language)
}

add_igraph_stats <- function (g, directed = TRUE) {

    g_igr <- igraph::graph_from_data_frame (g [, c ("from", "to")],
                                            directed = directed)

    cl <- igraph::clusters (g_igr)
    index <- match (g$from, names (cl$membership))
    if (directed)
        nms <- c ("cluster_dir", "centrality_dir")
    else
        nms <- c ("cluster_undir", "centrality_undir")
    g [nms [1]] <- cl$membership [index]
    btw <- igraph::betweenness (g_igr)
    g [nms [2]] <- btw [match (g$from, names (btw))]

    return (g)
}

#' convert tags_src into same format as the function summary of R code
#' @param tags tags_src from main fn
#' @noRd
src_stats <- function (tags) {

    res <- data.frame (file_name = NA_character_,
                       fn_name = NA_character_,
                       kind = NA_character_,
                       language = NA_character_,
                       loc = 0L,
                       npars = NA_integer_,
                       has_dots = NA)

    if (!is.null (tags)) {

        res <- data.frame (file_name = tags$file,
                           fn_name = tags$tag,
                           kind = tags$kind,
                           language = gsub ("^language:", "", tags$language),
                           loc = tags$end - tags$start + 1,
                           npars = NA_integer_,
                           has_dots = NA)
        res <- res [which (!is.na (res$loc)), ]
    }

    return (res)
}

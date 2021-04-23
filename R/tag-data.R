
#' use ctags and gtags to parse call data
#'
#' @param path Path to local repository
#' @export
tags_data <- function (path) {

    kind <- start <- NULL # no visible binding messages

    tags_r <- withr::with_dir (path, get_ctags ("R"))
    tags_src <- withr::with_dir (path, get_ctags ("src"))
    tags_inst <- withr::with_dir (path, get_ctags ("inst"))
    # does the code contain tab ("\t") characters?
    has_tabs <- files_have_tabs (tags_r) |
        files_have_tabs (tags_src) |
        files_have_tabs (tags_inst)

    gtags <- NULL

    if (!is.null (tags_src) | !is.null (tags_inst)) {

        gtags <- withr::with_dir (path, get_gtags ())
        ctags <- dplyr::arrange (rbind (tags_src, tags_inst), file, start)
        ctags <- dplyr::filter (ctags, kind %in% c ("class", "function", "struct"))
        gtags$from <- NA_character_
        for (f in unique (ctags$file))
            gtags <- gtags_from_one_file (ctags, gtags, f)
        gtags <- gtags [which (gtags$tag %in% ctags$tag), ]

        langs <- tags_src [, c ("tag", "language")]
        langs <- langs [which (!duplicated (langs)), ]
        gtags$language <- langs$language [match (gtags$tag, langs$tag)]
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
                  has_tabs = has_tabs))
}

#' Get tags for one directory within a package
#' @param d the directory
#' @noRd
get_ctags <- function (d = "R") {

    d <- match.arg (d, c ("R", "src", "inst"))

    path_dir <- file.path (getwd (), d)
    if (d == "inst")
        path_dir <- file.path (path_dir, "include")
    if (!dir.exists (path_dir))
        return (NULL)

    # tab-characters muck up parsing of tag content so have to be removed:
    has_tabs <- rm_tabs (path_dir)

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

    f <- tempfile (fileext = ".txt")
    cmd <- paste0 ("ctags -R --fields=",
                   fields,
                   " -f ",
                   f,
                   " ",
                   path_dir)
    system (cmd)

    # remove header lines:
    x <- brio::read_lines (f)
    x <- x [-which (grepl ("^\\!", x))]
    writeLines (x, con = f)

    ctypes <- list (readr::col_character (),
                    readr::col_character (),
                    readr::col_character (),
                    readr::col_character (),
                    readr::col_character (),
                    readr::col_character (),
                    readr::col_character ())
    cnames <- c ("tag", "file", "content", "kind", "start", "language", "end")

    suppressWarnings (
                      tags <- readr::read_delim (f,
                                                 delim = "\t",
                                                 col_names = cnames,
                                                 col_types = ctypes)
                      )


    tags$start <- as.integer (gsub ("^line\\:", "", tags$start))
    tags$end <- as.integer (gsub ("^end\\:", "", tags$end))
    tags$file <- gsub (paste0 (getwd (), .Platform$file.sep), "", tags$file)

    attr (tags, "has_tabs") <- has_tabs

    return (tags)
}

rm_tabs <- function (d) {

    files <- normalizePath (list.files (d,
                                        full.names = TRUE,
                                        recursive = TRUE))

    has_tabs <- FALSE 

    for (f in files) {
        x <- suppressWarnings (brio::read_lines (f))
        has_tabs_f <- any (grepl ("\\t", x))
        if (has_tabs_f) {
            x <- gsub ("\\t", " ", x)
            writeLines (x, con = f)
        }
        has_tabs <- has_tabs | has_tabs_f
    }

    return (has_tabs)
}

get_gtags <- function () {

    system ("gtags")
    x <- system ("global -rx  .", intern = TRUE)
    # these are fixed width, but only have 4 cols, so can just replace the first
    # 3 lots of space with single tab characters.
    for (i in 1:3)
        x <- sub (" +", "\t", x)

    ctypes <- list (readr::col_character (),
                    readr::col_double (),
                    readr::col_character (),
                    readr::col_character ())
    suppressWarnings (
        gtags <- readr::read_delim (x,
                                    delim = "\t",
                                    col_names = c ("tag", "line", "file", "content"),
                                    col_types = ctypes)
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
    # tags are duplicated for things like class constructors. The duplications are
    # always embedded within the main definition, so simply removing them reduces
    # line ranges to main definition only.
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

fn_var_call_graph_r <- function (fns, fn_vars, path) {

    res <- NULL

    for (f in unique (fns$file)) {

        fns_f <- fns [fns$file == f, ]
        fns_f <- fns_f [order (fns_f$start), c ("tag", "start", "end")]
        fns_index <- lapply (seq (nrow (fns_f)), function (i)
                             cbind (i, seq (fns_f$start [i], fns_f$end [i])))
        fns_index <- do.call (rbind, fns_index)

        f_full <- normalizePath (file.path (path, f))
        # note: keep.source must be TRUE as it is, for example, switched off in
        # `rmarkdown` environments, which means no parse data are returned by
        # getParseData.
        pd <- utils::getParseData (parse (file = f_full,
                                          keep.source = TRUE))
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
    v <- igraph::V (g_igr)

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

    res <- data.frame (file_name = tags$file,
                       fn_name = tags$tag,
                       kind = tags$kind,
                       language = gsub ("^language:", "", tags$language),
                       loc = tags$end - tags$start + 1,
                       npars = NA_integer_,
                       has_dots = NA)
    res <- res [which (!is.na (res$loc)), ]
}

#' @param x result of one get_ctags call, which may be `NULL`
#' @noRd
files_have_tabs <- function (x) {

    res <- FALSE
    if (!is.null (x))
        res <- attr (x, "has_tabs")
    if (is.null (res))
        res <- FALSE

    return (res)
}

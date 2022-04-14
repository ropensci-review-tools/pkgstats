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

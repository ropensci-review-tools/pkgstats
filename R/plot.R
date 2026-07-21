#' Plot interactive \pkg{visNetwork} visualisation of object-relationship
#' network of package.
#'
#' @param s Package statistics obtained from \link{pkgstats} function.
#' @param fn Optional name of function. If specified, plot only network
#' connections to or from the specified function.
#' @param plot If `TRUE`, plot the network using \pkg{visNetwork} which opens an
#' interactive browser pane.
#' @param vis_save Name of local file in which to save `html` file of network
#' visualisation (will override `plot` to `FALSE`).
#' @return (Invisibly) A \pkg{visNetwork} representation of the package network.
#'
#' @note Edge thicknesses are scaled to centrality within the package function
#' call network. Node sizes are scaled to numbers of times each function is
#' called from all other functions within a package.
#'
#' @family output
#' @examples
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' \donttest{
#' p <- pkgstats (f)
#' plot_network (p)
#' }
#' @export
plot_network <- function (s, fn = NULL, plot = TRUE, vis_save = NULL) {

    # suppress no visible binding notes:
    kind <- NULL

    network <- s$network
    rm_isolated_nodes <- FALSE
    if (!is.null (fn)) {
        checkmate::assert_character (fn)
        if (!all (fn %in% s$objects$fn_name)) {
            stop ("fn '", fn, "' not found.")
        }
        index <- which (s$network$from %in% fn | s$network$to %in% fn)
        network <- network [index, , drop = FALSE]
        rm_isolated_nodes <- TRUE
    }

    requireNamespace ("visNetwork", quietly = TRUE)

    if (!all (c (
        "loc", "vignettes", "data_stats", "desc",
        "translations", "objects", "network"
    ) %in% names (s))) {

        stop (
            "'s' must be a 'pkgstats' object obtained ",
            "from the 'pkgstats' function"
        )
    }

    pkg_title <- paste0 (s$desc$package, " network")

    if (nrow (network) == 0L && nrow (s$objects) == 0L) {

        return (NULL)

    } else if (nrow (network) == 0L) {

        # pkg has no internal fn_calls, so plot network of vertices only (#12)
        index <- which (s$objects$kind == "function" &
            !grepl ("^anonFunc", s$objects$fn_name))
        nodes <- s$objects [index, ]
        nodes <- nodes [which (!duplicated (nodes$fn_name)), ]
        nodes <- data.frame (
            id = nodes$fn_name,
            label = nodes$fn_name,
            name = nodes$fn_name,
            group = nodes$language,
            value = 1L,
            stringsAsFactors = FALSE
        )

        vn <- visNetwork::visNetwork (nodes, main = pkg_title)

    } else {

        edges <- data.frame (
            from = network$from,
            to = network$to,
            centrality = network$centrality_undir,
            language = network$language,
            stringsAsFactors = FALSE
        )
        from <- language <- centrality <- NULL # suppress no visible binding msg
        edges <- dplyr::count (edges, from, to, language, centrality)
        edges <- edges [which (!is.na (edges$from)), ]

        obj <- dplyr::filter (s$objects, kind == "function")
        obj <- dplyr::filter (obj, !grepl ("^anonFunc", obj$fn_name))
        obj <- obj [which (!duplicated (obj$fn_name)), ]
        nodes <- data.frame (
            id = obj$fn_name,
            label = obj$fn_name,
            name = obj$fn_name,
            language = obj$language,
            stringsAsFactors = FALSE
        )
        nodes$group <- nodes$language

        to <- NULL # no visible binding
        n <- dplyr::count (edges, to)
        nodes <- dplyr::left_join (nodes, n, by = c ("name" = "to"))
        nodes$value <- nodes$n
        nodes$value [is.na (nodes$value)] <- 1

        if (rm_isolated_nodes) {
            index <- which (nodes$name %in% c (network$from, network$to))
            nodes <- nodes [index, , drop = FALSE]
        }

        edges$width <- edges$centrality * 10 / max (edges$centrality)

        txt <- paste0 (
            "Edge thickness scaled to network centrality<br>",
            "Node sizes scaled to numbers of times each fn is called"
        )
        vn <- visNetwork::visNetwork (
            nodes,
            edges,
            main = pkg_title,
            submain = list (text = txt)
        )
        arrows <- list (to = list (enabled = TRUE, scaleFactor = 0.2))
        vn <- visNetwork::visEdges (vn, arrows = arrows)
    }

    vn <- visNetwork::visLegend (vn, main = "Language")

    if (plot || !is.null (vis_save)) {

        if (!is.null (vis_save)) {
            if (!is.character (vis_save)) {
                stop ("vis_save must be a character specifying a file name")
            }
            if (length (vis_save) > 1) {
                stop ("vis_save must be a single character")
            }
            if (!fs::dir_exists (fs::path_dir (vis_save))) {
                stop (
                    "directory [", fs::path_dir (vis_save),
                    "] does not exist"
                )
            }

            vis_save <- fs::path_ext_set (vis_save, "html")
            path <- fs::path_dir (vis_save)
            if (!fs::dir_exists (path)) {
                fs::dir_create (path, recurse = TRUE)
            }
            visNetwork::visSave (vn, vis_save, selfcontained = TRUE)

        } else {
            print (vn)
        }
    }

    invisible (vn)
}

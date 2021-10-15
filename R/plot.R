
#' Plot interactive \pkg{visNetwork} visualisation of object-relationship
#' network of package.
#'
#' @param s Package statistics obtained from \link{pkgstats} function.
#' @param plot If `TRUE`, plot the network using \pkg{visNetwork} which opens an
#' interactive browser pane.
#' @param vis_save Name of local file in which to save `html` file of network
#' visualisation (will override `plot` to `FALSE`).
#' @return (Invisibly) A \pkg{visNetwork} representation of the package network.
#' @family output
#' @export
plot_network <- function (s, plot = TRUE, vis_save = NULL) {

    requireNamespace ("visNetwork")

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

    if (nrow (s$network) == 0L && nrow (s$objects) == 0L) {

        return (NULL)

    } else if (nrow (s$network) == 0L) {

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
            value = 1L
        )

        vn <- visNetwork::visNetwork (nodes, main = pkg_title)

    } else {

        edges <- data.frame (
            from = s$network$from,
            to = s$network$to,
            centrality = s$network$centrality_undir,
            language = s$network$language
        )
        from <- language <- centrality <- NULL # suppress no visible binding msg
        edges <- dplyr::count (edges, from, to, language, centrality)
        edges <- edges [which (!is.na (edges$from)), ]
        nodes <- unique (c (edges$from, edges$to))
        nodes <- data.frame (
            id = nodes,
            label = nodes,
            name = nodes
        )

        # match languages on to nodes:
        langs <- rbind (
            data.frame (
                n = edges$from,
                lang = edges$language
            ),
            data.frame (
                n = edges$to,
                lang = edges$language
            )
        )
        langs <- langs [which (!duplicated (langs)), ]
        nodes$group <- langs$lang [match (nodes$id, langs$n)]

        to <- NULL # no visible binding
        n <- dplyr::count (edges, to)
        nodes <- dplyr::left_join (nodes, n, by = c ("name" = "to"))
        nodes$value <- nodes$n
        nodes$value [is.na (nodes$value)] <- 1

        edges$width <- edges$centrality * 10 / max (edges$centrality)

        vn <- visNetwork::visNetwork (nodes, edges, main = pkg_title)
        arrows <- list (to = list (enabled = TRUE, scaleFactor = 0.2))
        vn <- visNetwork::visEdges (vn, arrows = arrows)
    }

    vn <- visNetwork::visLegend (vn, main = "Language")

    if (plot | !is.null (vis_save)) {

        if (!is.null (vis_save)) {
            if (!is.character (vis_save)) {
                  stop ("vis_save must be a character specifying a file name")
              }
            if (length (vis_save) > 1) {
                  stop ("vis_save must be a single character")
              }
            if (!dir.exists (dirname (vis_save))) {
                  stop (
                      "directory [", dirname (vis_save),
                      "] does not exist"
                  )
              }

            vis_save <- paste0 (
                tools::file_path_sans_ext (vis_save),
                ".html"
            )
            path <- strsplit (vis_save, .Platform$file.sep) [[1]]
            # can't use normalizePath because that fails if path does not exist
            path <- paste0 (path [-length (path)],
                collapse = .Platform$file.sep
            )
            if (!file.exists (path)) {
                  dir.create (path, recursive = TRUE)
              }
            visNetwork::visSave (vn, vis_save, selfcontained = TRUE)

        } else {
            print (vn)
        }
    }

    invisible (vn)
}

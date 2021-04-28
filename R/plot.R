
#' Plot interactive \pkg{visNetwork} visualisation of object-relationship
#' network of package.
#'
#' @param s Package statistics obtained from \link{pkgstats} function.
#' @return (Invisibly) A \pkg{visNetwork} representation of the package network.
#' @export
plot_network <- function (s) {

    requireNamespace ("visNetwork")

    if (!all (c ("cloc", "vignettes", "data_stats", "desc",
                 "translations", "code_has_tabs",
                 "objects", "network") %in% names (s))) {

        stop ("'s' must be a 'pkgstats' object obtained ",
              "from the 'pkgstats' function")
    }

    edges <- data.frame (from = s$network$from,
                         to = s$network$to,
                         centrality = s$network$centrality_undir,
                         language = s$network$language)
    from <- language <- centrality <- NULL # suppress no visible binding msg
    edges <- dplyr::count (edges, from, to, language, centrality)
    edges <- edges [which (!is.na (edges$from)), ]
    nodes <- unique (c (edges$from, edges$to))
    nodes <- data.frame (id = nodes,
                         label = nodes,
                         name = nodes)

    # match languages on to nodes:
    langs <- rbind (data.frame (n = edges$from,
                                lang = edges$language),
                    data.frame (n = edges$to,
                                lang = edges$language))
    langs <- langs [which (!duplicated (langs)), ]
    nodes$group <- langs$lang [match (nodes$id, langs$n)]

    to <- NULL # no visible binding
    n <- dplyr::count (edges, to)
    nodes <- dplyr::left_join (nodes, n, by = c ("name" = "to"))
    nodes$value <- nodes$n
    nodes$value [is.na (nodes$value)] <- 1

    edges$width <- edges$centrality * 10 / max (edges$centrality)
    pkg_title <- paste0 (s$desc$package, " network")
    vn <- visNetwork::visNetwork (nodes, edges,
                                  main = pkg_title)
    arrows <- list (to = list (enabled = TRUE, scaleFactor = 0.2))
    vn <- visNetwork::visEdges (vn, arrows = arrows)
    vn <- visNetwork::visLegend (vn, main = "Language")

    print (vn)

    invisible (vn)
}

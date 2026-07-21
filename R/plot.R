#' Plot interactive visualisation of object-relationship network of package.
#'
#' @param s Package statistics obtained from \link{pkgstats} function.
#' @param plot If `TRUE`, plot the network which opens an interactive browser
#' pane.
#' @param vis_save Name of local file in which to save `html` file of network
#' visualisation (will override `plot` to `FALSE`).
#' @return (Invisibly) A `pkgstats_network` object: a list of the `nodes` and
#' `edges` used to construct the network, along with the self-contained
#' `html` document itself.
#'
#' @note Edge thicknesses are scaled to centrality within the package function
#' call network. Node sizes are scaled to numbers of times each function is
#' called from all other functions within a package. The visualisation is
#' rendered with a small bundle of \pkg{D3}-based JavaScript shipped with
#' this package (see `system.file ("js", package = "pkgstats")`), so no
#' internet connection or additional R packages are required to view it.
#'
#' @family output
#' @examples
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' \donttest{
#' p <- pkgstats (f)
#' net <- plot_network (p, plot = FALSE)
#' }
#' # use default 'plot = TRUE' to automatically open network in browser.
#' @export
plot_network <- function (s, plot = TRUE, vis_save = NULL) {

    # suppress no visible binding notes:
    kind <- NULL

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
            language = nodes$language,
            group = nodes$language,
            n = NA_integer_,
            value = 1L,
            file = nodes$file_name,
            stringsAsFactors = FALSE
        )
        edges <- data.frame (
            from = character (0),
            to = character (0),
            language = character (0),
            centrality = numeric (0),
            n = integer (0),
            width = numeric (0),
            stringsAsFactors = FALSE
        )
        subtitle <- ""

    } else {

        edges <- data.frame (
            from = s$network$from,
            to = s$network$to,
            centrality = s$network$centrality_undir,
            language = s$network$language,
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
            file = obj$file_name,
            stringsAsFactors = FALSE
        )
        nodes$group <- nodes$language

        to <- NULL # no visible binding
        n <- dplyr::count (edges, to)
        nodes <- dplyr::left_join (nodes, n, by = c ("name" = "to"))
        nodes$value <- nodes$n
        nodes$value [is.na (nodes$value)] <- 1

        edges$width <- edges$centrality * 10 / max (edges$centrality)

        subtitle <- paste0 (
            "Edge thickness scaled to network centrality &middot; ",
            "node sizes scaled to numbers of times each fn is called"
        )
    }

    html <- pkgstats_network_html (
        nodes, edges,
        title = pkg_title, subtitle = subtitle
    )

    out <- list (nodes = nodes, edges = edges, html = html)
    class (out) <- c ("pkgstats_network", class (out))

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
            brio::write_file (html, vis_save)

        } else {
            tmp <- fs::file_temp (ext = "html")
            brio::write_file (html, tmp)
            utils::browseURL (tmp)
        }
    }

    invisible (out)
}

#' Assemble the self-contained network-plot `html` document from a bundled
#' `D3`-based JavaScript skeleton (see `inst/js`), populated with the given
#' `nodes` and `edges`.
#'
#' @noRd
pkgstats_network_html <- function (nodes, edges, title, subtitle = "") {

    requireNamespace ("jsonlite", quietly = TRUE)

    data <- list (
        nodes = nodes [, c ("id", "label", "group", "value", "file")],
        edges = edges [, c ("from", "to", "width")]
    )
    data_json <- jsonlite::toJSON (
        data,
        dataframe = "rows",
        auto_unbox = TRUE,
        na = "null"
    )

    js_path <- function (f) system.file ("js", f, package = "pkgstats")

    template <- brio::read_file (js_path ("template.html"))
    d3_js <- brio::read_file (js_path ("d3.v7.min.js"))
    network_js <- brio::read_file (js_path ("pkgstats-network.js"))
    network_css <- brio::read_file (js_path ("pkgstats-network.css"))

    html <- gsub ("__TITLE__", title, template, fixed = TRUE)
    html <- gsub ("__SUBTITLE__", subtitle, html, fixed = TRUE)
    html <- gsub ("__CSS__", network_css, html, fixed = TRUE)
    html <- gsub ("__D3_JS__", d3_js, html, fixed = TRUE)
    html <- gsub ("__NETWORK_JS__", network_js, html, fixed = TRUE)
    html <- gsub ("__DATA__", data_json, html, fixed = TRUE)

    html
}

#' Download latest version of 'pkgstats' data
#'
#' @param current If 'FALSE', download data for all CRAN packages ever released,
#' otherwise (default) download data only for current CRAN packages.
#' @param path Local path to download file.
#' @param quiet If `FALSE`, display progress information on screen.
#' @return (Invisibly) A `data.frame` of `pkgstats` results, one row for each
#' package.
#' @family archive
#' @export
dl_pkgstats_data <- function (current = TRUE,
                              path = tempdir (),
                              quiet = FALSE) {

    requireNamespace ("curl")
    requireNamespace ("jsonlite")

    u <- paste0 (
        "https://api.github.com/repos/",
        "ropensci-review-tools/pkgstats/",
        "releases/latest"
    )

    res <- curl::curl_fetch_memory (u)
    hdrs <- curl::parse_headers (res$headers)
    http_code <- as.integer (gsub (
        "^http\\/[0-9]\\s?|\\s+$",
        "",
        hdrs [1],
        ignore.case = TRUE
    ))
    if (http_code != 200L) {
        stop (
            "Call to GitHub failed with http error code [",
            http_code, "]"
        )
    }

    res <- jsonlite::fromJSON (rawToChar (res$content))
    assets <- res$assets

    i <- ifelse (
        current,
        grep ("current", assets$name, ignore.case = TRUE),
        grep ("all", assets$name, ignore.case = TRUE)
    )
    dl_url <- assets$browser_download_url [i]
    f <- file.path (path, basename (dl_url))

    curl::curl_download (url = dl_url, destfile = f, quiet = quiet)
    if (!quiet) {
        message ("Downloaded to [", f, "]")
    }

    invisible (readRDS (f))
}

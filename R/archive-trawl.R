
#' Trawl a local CRAN archive and extract statistics from all packages
#' @param path Path to local CRAN archive
#' @export
pkgstats_from_archive <- function (path) {

    if (!grepl ("tarball", path))
        path <- file.path (path, "tarballs")

    if (!dir.exists (path))
        stop ("[", path, "] directory does not exist")

    flist <- list.files (path,
                         recursive = TRUE,
                         full.names = TRUE,
                         pattern = "\\.tar\\.gz$")

    ncl <- parallel::detectCores () / 2
    clusters <- parallel::makeCluster (ncl)
    doParallel::registerDoParallel (clusters)

    res <- pbapply::pblapply (flist, function (i)
                              pkgstats::pkgstats_summary (pkgstats::pkgstats (i)),
                              cl = clusters)

    parallel::stopCluster (clusters)

    res <- do.call (rbind, res)

    saveRDS (res, "pkgstats-results.Rds")

    invisible (res)
}


#' Trawl a local CRAN archive and extract statistics from all packages
#' @param path Path to local CRAN archive
#' @param archive If `TRUE`, extract statistics for all packages in the
#' `/Archive` sub-directory, otherwise only statistics for main `tarballs`
#' directory (that is, current packages only).
#' @export
pkgstats_from_archive <- function (path, archive = TRUE) {

    if (!grepl ("tarball", path))
        path <- file.path (path, "tarballs")

    if (!dir.exists (path))
        stop ("[", path, "] directory does not exist")

    flist <- list.files (path,
                         recursive = archive,
                         full.names = TRUE,
                         pattern = "\\.tar\\.gz$")

    ncl <- parallel::detectCores () / 2
    clusters <- parallel::makeCluster (ncl)
    doParallel::registerDoParallel (clusters)

    res <- pbapply::pblapply (flist, function (i) {
                                  s <- tryCatch (pkgstats (i),
                                                 error = function (e) NULL)
                                  res <- NULL
                                  if (!is.null (s))
                                      res <- pkgstats_summary (s)
                                  return (res)
                         },
                         cl = clusters)

    parallel::stopCluster (clusters)

    res <- do.call (rbind, res)

    saveRDS (res, "pkgstats-results.Rds")

    invisible (res)
}


#' Trawl a local CRAN archive and extract statistics from all packages
#'
#' @param path Path to local CRAN archive
#' @param archive If `TRUE`, extract statistics for all packages in the
#' `/Archive` sub-directory, otherwise only statistics for main `tarballs`
#' directory (that is, current packages only).
#' @param prev_results Result of previous call to this function, if available.
#' Submitting previous results will ensure that only newer packages not present
#' in previous result will be analysed, with new results simply appended to
#' previous results.
#' @param results_file Can be used to specify the name or full path of a `.Rds`
#' file to which results should be saved once they have been generated. The
#' '.Rds' extension will be automatically appended, and any other extensions
#' will be ignored.
#'
#' @return A `data.frame` object with one row for each package containing
#' summary statistics generated from the \link{pkgstats_summary} function.
#'
#' @export
pkgstats_from_archive <- function (path, archive = TRUE, prev_results = NULL,
                                   results_file = NULL) {

    requireNamespace ("parallelly")
    requireNamespace ("future")
    requireNamespace ("future.apply")

    if (!grepl ("tarball", path)) {
        if (!dir.exists (file.path (path, "tarballs")))
            stop ("path must contain a 'tarballs' directory")
        path <- file.path (path, "tarballs")
    }

    path_last <- utils::tail (strsplit (path, .Platform$file.sep) [[1]], 1)
    if (path_last != "tarballs")
        stop ("path must be a directory named 'tarballs'")

    if (!dir.exists (path))
        stop ("[", path, "] directory does not exist")

    flist <- list.files (path,
                         recursive = archive,
                         full.names = TRUE,
                         pattern = "\\.tar\\.gz$")

    out <- prev_results

    res <- NULL

    if (!is.null (prev_results)) {

        prev_tars <- paste0 (prev_results$package,
                             "_",
                             prev_results$version,
                             ".tar.gz")
        index <- grepl (paste0 (prev_tars, "$", collapse = "|"), flist)

        flist <- flist [which (!index)]
    }

    if (length (flist) > 0) {

        future::plan (future::multisession (workers =
                                    parallelly::availableCores () / 2))

        res <- future.apply::future_lapply (flist, function (i) {
                                      s <- tryCatch (pkgstats (i),
                                                     error = function (e) NULL)
                                      res <- NULL
                                      if (!is.null (s))
                                          res <- tryCatch (pkgstats_summary (s),
                                                           error = function (e)
                                                               NULL)
                                      return (res)
                             },
                             future.seed = 1)

        future::plan (sequential) # close multisession workers

        res <- do.call (rbind, res)
    }

    out <- rbind (out, res)
    rownames (out) <- NULL

    if (!is.null (res) & !is.null (results_file)) {

        results_path <- gsub (basename (results_file), "",
                              results_file)
        if (!dir.exists (results_path))
            stop ("Directory [", results_path, "] does not exist")

        results_file <- paste0 (tools::file_path_sans_ext (results_file),
                                ".Rds")

        saveRDS (out, results_file)
    }

    invisible (out)
}

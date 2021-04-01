#' Collates statistics from one local tarball
#'
#' @inheritParams extract_tarball
#' @return List of statistics
#' @export
pkgstats <- function (tarball) {

    path <- extract_tarball (tarball)

    s1 <- cloc_stats (path)
    num_vignettes <- length (list.files (file.path (path, "vignettes")))
    s2 <- desc_stats (path)
    s3 <- rd_stats (path)

    fns <- all_functions (path)
    fns <- fns [which (!fns %in% s3$fn_name)]

    list (cloc = s1,
          num_vignettes = num_vignettes,
          desc = s2,
          rd = s3,
          non_exported_fns = fns)
}

#' Get all exported and internal functions
#' @noRd
all_functions <- function (path) {

    r_files <- normalizePath (list.files (file.path (path, "R"),
                                          full.names = TRUE))

    eval1 <- function (i, e) {
        p <- tryCatch (parse (file = i),
                       error = function (err) "error")
        if ("error" %in% p)
            return (NULL)

        eval (p, envir = e)
    }

    e <- new.env ()
    fns <- lapply (r_files, function (i) eval1 (i, e)) # nolint

    return (ls (envir = e))
}

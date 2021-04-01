#' Collates statistics from one local tarball
#'
#' @inheritParams extract_tarball
#' @return List of statistics
#' @export
#' @examples
#' \dontrun{
#' tarball <- "magrittr_2.0.1.tar.gz"
#' u <- paste0 ("https://cran.r-project.org/src/contrib/",
#'              tarball)
#' f <- file.path (tempdir (), tarball)
#' download.file (u, f)
#' pkgstats (f)
#' }
pkgstats <- function (tarball) {

    path <- extract_tarball (tarball)

    s1 <- cloc_stats (path)
    num_vignettes <- length (list.files (file.path (path, "vignettes")))
    s2 <- desc_stats (path)
    s3 <- rd_stats (path)

    fns <- all_functions (path)
    fns$exported <- FALSE
    index <- which (fns$fn_name %in% s3$fn_name)
    fns$exported [which (fns$fn_name %in% s3$fn_name)] <- TRUE

    # non-dplyr left_join:
    index2 <- match (fns$fn_name [index], s3$fn_name)
    fns$num_params <- fns$num_doclines <- NA_integer_
    fns$num_params [index] <- s3$num_params [index2]
    fns$num_doclines [index] <- s3$num_doclines [index2]

    list (cloc = s1,
          num_vignettes = num_vignettes,
          desc = s2,
          functions = fns)
}

#' Get all exported and internal functions
#' @noRd
all_functions <- function (path) {

    r_files <- normalizePath (list.files (file.path (path, "R"),
                                          full.names = TRUE))

    eval1 <- function (f) {
        p <- tryCatch (parse (file = f),
                       error = function (err) "error")
        if ("error" %in% p)
            return (NULL)

        # All functions are parsed to length 3, while things like `"_PACKAGE"`
        # statements are parsed, but have parse lengths < 3.
        p <- p [which (vapply (p, length, integer (1)) > 2L)]

        loc <- vapply (p, function (i)
                       length (deparse (i)),
                       integer (1))
        nms <- vapply (p, function (i)
                       paste0 (as.list (i) [[2]]),
                       character (1))

        data.frame (fn_name = nms,
                    loc = loc)
    }

    do.call (rbind, lapply (r_files, eval1))
}

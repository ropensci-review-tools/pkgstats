
get_num_vignettes <- function (path) {

    nv <- 0

    if ("build" %in% list.files (path)) {

        flist <- list.files (file.path (path, "build"),
                             full.names = TRUE)
        vfile <- grep ("vignette", flist, value = TRUE)
        if (length (vfile) > 0) {
            nv <- nrow (readRDS (vfile [1]))
        }
    } else if ("vignettes" %in% list.files (path)) {
        nv <- length (list.files (file.path (path, "vignettes"),
                                  pattern = "\\.R"))
    }

    return (nv)
}

get_num_demos <- function (path) {

    nd <- 0

    if ("demo" %in% list.files (path)) {
        
        dindex <- file.path (path, "demo", "00Index")
        nd <- length (readLines (dindex))
    }

    return (nd)
}

get_data_stats <- function (path) {

    nd <- c (n = 0L, total_size = 0L, median_size = 0L)

    if ("data" %in% list.files (path)) {

        flist <- list.files (file.path (path, "data"),
                             full.names = TRUE,
                             pattern = ".rda")
        sizes <- vapply (flist, function (i)
                         file.info (i)$size,
                         numeric (1),
                         USE.NAMES = FALSE)

        nd [1] <- length (flist)
        nd [2] <- as.integer (sum (sizes))
        nd [3] <- as.integer (stats::median (sizes))
    }

    return (nd)
}

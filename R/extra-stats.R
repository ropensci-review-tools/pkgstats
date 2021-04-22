
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
        nd <- length (brio::read_lines (dindex))
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

# https://cran.r-project.org/doc/manuals/R-exts.html#Preparing-translations
# https://cran.r-project.org/doc/manuals/R-admin.html#Localization-of-messages
# https://developer.r-project.org/TranslationTeams.html
get_translations <- function (path) {

    ll <- NA_character_

    po_dir <- normalizePath (file.path (path, "po"), mustWork = FALSE)

    if (file.exists (po_dir)) {

        ll <- gsub ("^R-|\\.po$", "",
                    list.files (po_dir, pattern = "\\.po$"))
    }

    return (unique (ll))
}

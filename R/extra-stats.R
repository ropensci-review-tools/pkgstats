get_num_vignettes <- function (path) {

    nv <- 0L

    path_ls <- basename (fs::dir_ls (path))

    if ("build" %in% path_ls) {

        flist <- fs::dir_ls (fs::path (path, "build"),
            full.names = TRUE
        )
        vfile <- grep ("vignette", flist, value = TRUE)
        if (length (vfile) > 0) {
            nv <- nrow (readRDS (vfile [1]))
        }
    } else if ("vignettes" %in% path_ls) {
        nv <- length (fs::dir_ls (
            fs::path (path, "vignettes"),
            regexp = "\\.[rR]md$"
        ))
        if (nv == 0L) {
            nv <- length (fs::dir_ls (
                fs::path (path, "vignettes"),
                regexp = "\\.[rR]$"
            ))
        }
    }

    return (nv)
}

get_num_demos <- function (path) {

    nd <- 0L

    path_ls <- basename (fs::dir_ls (path))

    if ("demo" %in% path_ls) {

        dindex <- fs::path (path, "demo", "00Index")
        nd <- length (brio::read_lines (dindex))
    }

    return (nd)
}

get_data_stats <- function (path) {

    nd <- c (n = 0L, total_size = 0L, median_size = 0L)

    path_ls <- basename (fs::dir_ls (path))

    if ("data" %in% path_ls) {

        flist <- fs::dir_ls (fs::path (path, "data"),
            regexp = "\\.rd",
            ignore.case = TRUE
        )
        sizes <- fs::file_info (flist)$size

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

    po_dir <- expand_path (fs::path (path, "po"))

    if (fs::dir_exists (po_dir)) {

        ll <- gsub (
            "^R-|\\.po$", "",
            fs::dir_ls (po_dir, regexp = "\\.po$")
        )
        # pkgs may generate translations without having any, and then will only
        # have '.pot' files with no usable translation fields.
        if (length (ll) == 0L) {
            ll <- NA_character_
        }
    }

    return (unique (ll))
}

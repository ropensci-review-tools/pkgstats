# Mostly only lightly adapted from
# https://github.com/stan-dev/cmdstanr/blob/master/R/install.R

#' Return URL of zip file for latest ctags version
#' @noRd
latest_ctags_version <- function() {

    gh_url <- "https://api.github.com/repos/"
    dest_file <- file.path (tempdir (), "universal-ctags-releases.json")

    if (!file.exists (dest_file)) {

        tags_url <- paste0 (gh_url, "universal-ctags/ctags/git/refs/tags")

        release_list_downloaded <- download_with_retries(tags_url, dest_file)
        if (!release_list_downloaded) {
            stop("GitHub download of release list failed.", call. = FALSE)
        }
    }

    latest_release <- jsonlite::read_json(dest_file) [[1]]
    version <- latest_release$ref

    paste0 ("https://github.com/universal-ctags/ctags/archive/",
            version, ".tar.gz")
}

download_ctags <- function (destfile = NULL) {

    if (is.null (destfile)) {
        stop ("destfile must be specified", call. = FALSE)
    }
    destfile <- normalizePath (destfile, mustWork = FALSE)
    if (!dir.exists (dirname (destfile))) {
        stop ("Directory [", dirname (destfile),
              "] does not exist", call. = FALSE)
    }

    u <- latest_ctags_version ()
    ctags_downloaded <- download_with_retries (u, destfile)
    if (!ctags_downloaded)
        stop ("GitHub download of ctags zip archive failed.", call. = FALSE)

    dir_ctags <- file.path (dirname (destfile), "ctags")
    z <- utils::untar (destfile,
                       exdir = dir_ctags,
                       extras = "--strip-components 1")

    if (z != 0)
        stop ("Failed to extract ctags archive", call. = FALSE)

    return (dir_ctags)
}

# download with retries and pauses
download_with_retries <- function(download_url,
                                  destination_file,
                                  retries = 5,
                                  pause_sec = 5,
                                  quiet = TRUE) {

    download_rc <- 1
    while (retries > 0 && download_rc != 0) {
        try(
            suppressWarnings(
                download_rc <- utils::download.file(url = download_url,
                                                    destfile = destination_file,
                                                    quiet = quiet)
                ),
            silent = TRUE
        )
        if (download_rc != 0) {
            Sys.sleep(pause_sec)
        }
        retries <- retries - 1
    }
    if (download_rc == 0) {
        TRUE
    } else {
        FALSE
    }
}

#' Make install ctags from extracted .tar.gz in 'ctags_dir', returned from
#' `download_ctags()`.
#' @note Requires 'sudo' and will fail if not
#' @noRd
ctags_make <- function (ctags_dir) {

    f <- tempfile (pattern = "ctags-make-", fileext = ".txt")

    withr::with_dir (ctags_dir, {
        sys::exec_wait ("./autogen.sh", std_out = f)
        sys::exec_wait ("./configure", args = c ("prefix=/usr"),
                        std_out = f)
        sys::exec_wait ("make", std_out = f)
        sys::exec_wait ("sudo", args = c ("make", "install"),
                        std_out = f)
        })
}


get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools") # nolint

#' Check path is an existing directory
#' @noRd
check_path <- function (path) {

    if (!file.exists (path))
        stop (paste0 ("path [", path, "] does not exist. ",
                      "Did you first 'extract_tarball()'?"))
}

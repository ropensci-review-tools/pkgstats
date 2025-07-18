#' Get all \pkg{roxygen2} blocks from R directory
#'
#' This is used only to get the documentation stats for non-exported functions.
#' @noRd
roxygen_block_stats <- function (path) {

    r_files <- fs::dir_ls (fs::path (path, "R"), regexp = "\\.(R|r|q)$")
    blocks <- lapply (r_files, function (f) {
        suppressMessages (
            tryCatch (
                roxygen2::parse_file (f, env = NULL),
                error = function (e) NULL
            )
        )
    })
    names (blocks) <- r_files
    blocks <- do.call (c, blocks)

    file_names <- gsub ("[0-9]*$", "", names (blocks))
    file_names <- as.character (fs::path_rel (file_names, path))
    fn_names <- vapply (blocks, function (b) {
        as.character (b$call) [2]
    }, character (1L), USE.NAMES = FALSE)

    exported <- vapply (blocks, function (b) {
        length (roxygen2::block_get_tags (b, "export")) > 0L
    }, logical (1L))

    num_doclines <- vapply (blocks, function (b) {
        lines_b <- vapply (b$tags, function (i) i$line, numeric (1L))
        as.integer (diff (range (lines_b)))
    }, integer (1L), USE.NAMES = FALSE)
    num_param_chars <- vapply (blocks, function (b) {
        pars <- roxygen2::block_get_tags (b, "param")
        par_txt <- vapply (pars, function (p) {
            p$val$description
        }, character (1L))
        ret <- roxygen2::block_get_tags (b, "returns")
        ret_txt <- vapply (ret, function (p) {
            p$val
        }, character (1L))

        nchars <- 0
        if (length (par_txt) > 0L || length (ret_txt) > 0L) {
            nchars <- nchar (c (par_txt, ret_txt))
        }
        return (c (md = stats::median (nchars), mn = mean (nchars)))
    }, numeric (2L), USE.NAMES = FALSE)

    data.frame (
        file_name = file_names,
        fn_name = fn_names,
        exported = exported,
        param_nchars_md = num_param_chars [1, ],
        param_nchars_mn = num_param_chars [2, ],
        num_doclines = num_doclines,
        row.names = NULL
    )
}

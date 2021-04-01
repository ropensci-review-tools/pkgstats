#' Stats from '.Rd' files
#'
#' @inheritParams cloc_stats
#' @export
rd_stats <- function (path) {

    check_path (path)

    rd_files <- list.files (file.path (path, "man"),
                            pattern = "\\.Rd$",
                            full.names = TRUE)

    params <- do.call (rbind, lapply (rd_files, get_one_params))
    params_sp <- split (params, f = factor (params$alias))

    n <- vapply (params_sp, function (i) {
                ret <- 0L
                if (nrow (i) > 1 | i$parameter [1] != "(none)")
                    ret <- nrow (i)
                return (ret)    },
                integer (1))
    nchars <- lapply (params_sp, function (i) nchar (i$description))
    nchars_mn <- vapply (nchars, mean, numeric (1))
    nchars_md <- vapply (nchars, median, numeric (1))

    # excluce imported fns:
    nmspc <- file.path (path, "NAMESPACE")
    checkmate::assert_file (nmspc)
    nmspc <- readLines (nmspc)

    imports <- grep ("^importFrom", nmspc, value = TRUE)
    imports <- unlist (lapply (strsplit (imports, ","),
                               function (i)
                                   gsub ("\\)$", "", i [2])))
    imports <- gsub ("\\\"", "", imports)

    if (any (imports %in% names (n))) {
        index <- which (!names (n) %in% imports)
        n <- n [index]
        nchars_mn <- nchars_mn [index]
        nchars_md <- nchars_md [index]
    }

    doclines <- vapply (names (n), function (i)
                        params$doclines [params$alias == i] [1],
                        integer (1))

    ret <- data.frame (fn_name = names (n),
                       num_params = unname (n),
                       num_doclines = doclines,
                       param_nchars_mn = nchars_mn,
                       param_nchars_md = nchars_md,
                       row.names = NULL)

    return (ret)
}

rd_is_fn <- function (rd) {

    tags <- vapply (rd, function (i) attr (i, "Rd_tag"), character (1))
    tags <- gsub ("\\\\", "", grep ("^\\\\", tags, value = TRUE))

    is_fn <- TRUE
    if ("docType" %in% tags) {
        is_fn <- !get_Rd_metadata (rd, "docType") %in% c ("data", "package")
    }
    return (is_fn)
}

get_one_params <- function (man_file) {

    res <- NULL
    rd <- tools::parse_Rd (man_file)

    out <- utils::capture.output (tools::Rd2txt (rd))
    doclines <- length (out [out != ""])

    if (!rd_is_fn (rd))
        return (res)

    aliases <- unique (c (get_Rd_metadata (rd, "name"),
                          get_Rd_metadata (rd, "alias")))

    params <- get_Rd_metadata (rd, "arguments")
    if (length (params) == 0) {

        res <- data.frame (parameter = "(none)",
                           description = "",
                           alias = aliases)
    } else {

        # This is not an accurate estimate of number of parameters; the real
        # value is extracted in the main `all_functions` fn.
        params <- strsplit (params, "\\n\\n\\n") [[1]]
        params <- lapply (params, function (i) {
                              ret <- unlist (eval (parse (text = i)))
                              ret <- c (ret [1],
                                        paste0 (ret [-1], collapse = " "))
                              return (ret)  })
        params <- data.frame (do.call (rbind, params))
        names (params) <- c ("parameter", "description")

        res <- lapply (aliases, function (i) {
                           params$alias <- i
                           return (params)  })

        res <- do.call (rbind, res)
    }

    res$doclines <- doclines

    return (res)
}

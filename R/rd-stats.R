#' Stats from '.Rd' files
#'
#' @inheritParams loc_stats
#' @return A `data.frame` of function names and numbers of parameters and lines
#' of documentation for each, along with mean and median numbers of characters
#' used to document each parameter.
#' @family stats
#' @export
#' @examples
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' # have to extract tarball to call function on source code:
#' path <- extract_tarball (f)
#' rd_stats (path)
rd_stats <- function (path) {

    path <- check_path (path)

    rd_files <- fs::dir_ls (fs::path (path, "man"), regexp = "\\.Rd$")

    suppressWarnings (
        params <- lapply (rd_files, get_one_params)
    )
    params <- do.call (rbind, params)
    params <- params [which (!is.na (params$parameter)), ]
    params_sp <- NULL
    if (!is.null (params)) {
        params_sp <- split (params, f = factor (params$alias))
    }

    n <- vapply (
        params_sp, function (i) {
            ret <- 0L
            if (nrow (i) > 1 | i$parameter [1] != "(none)") {
                ret <- nrow (i)
            }
            return (ret)    },
        integer (1)
    )

    nchars_mn <- vapply (params_sp, function (i) {
        mean (i$nchar)
    }, numeric (1))
    nchars_md <- vapply (params_sp, function (i) {
        stats::median (i$nchar)
    }, numeric (1))

    # excluce imported fns:
    nmspc <- fs::path (path, "NAMESPACE")
    # some packages have no NAMESPACE files (like adehabitat 1.2-1)
    if (file.exists (nmspc)) {

        nmspc <- brio::read_lines (nmspc)

        imports <- grep ("^importFrom", nmspc, value = TRUE)
        imports <- unlist (lapply (
            strsplit (imports, ","),
            function (i) {
                gsub ("\\)$", "", i [2])
            }
        ))
        imports <- gsub ("\\\"", "", imports)

        if (any (imports %in% names (n))) {
            index <- which (!names (n) %in% imports)
            n <- n [index]
            nchars_mn <- nchars_mn [index]
            nchars_md <- nchars_md [index]
        }
    }

    doclines <- vapply (
        names (n), function (i) {
            params$doclines [params$alias == i] [1]
        },
        integer (1)
    )

    ret <- data.frame (
        fn_name = names (n),
        num_params = unname (n),
        num_doclines = doclines,
        param_nchars_mn = nchars_mn,
        param_nchars_md = nchars_md,
        row.names = NULL,
        stringsAsFactors = FALSE
    )

    return (ret)
}

rd_is_fn <- function (rd) {

    if (is.null (rd)) {
        return (FALSE)
    }

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

    # rd <- tools::parse_Rd (man_file)
    # Rd comments (per sec 2.1 of Extensions manual) can muck up parsing, so
    # must be removed
    x <- brio::read_lines (man_file)
    index1 <- grep ("[^0-9]%", x)
    index2 <- grep ("\\\\%", x)
    index <- index1 [which (!index1 %in% index2)]
    x [index] <- gsub ("%.*$", "", x [index])
    ptn <- paste0 ("Rdtemp-", Sys.getpid (), "-")
    f <- fs::file_temp (pattern = ptn, ext = ".Rd")
    brio::write_lines (x, f)
    rd <- tryCatch (
        tools::parse_Rd (f),
        error = function (e) NULL
    )
    chk <- fs::file_delete (f) # nolint

    if (!rd_is_fn (rd)) {
        return (res)
    }

    out <- tryCatch (
        utils::capture.output (tools::Rd2txt (rd)),
        error = function (e) NULL
    )
    if (is.null (out)) {
        # For 'stages' param, see #40
        out <- tryCatch (
            utils::capture.output (tools::Rd2txt (rd, stages = "build")),
            error = function (e) NULL
        )
    }

    doclines <- length (out [out != ""])

    aliases <- unique (c (
        get_Rd_metadata (rd, "name"),
        get_Rd_metadata (rd, "alias")
    ))

    params <- get_Rd_metadata (rd, "arguments")
    if (length (params) == 0) {

        res <- data.frame (
            parameter = "(none)",
            nchar = NA_integer_,
            alias = aliases,
            doclines = doclines,
            stringsAsFactors = FALSE
        )
    } else {
        params <- strsplit (params, "\\n") [[1]]
        # rm lines with initial Rd comments, but params may still fail to parse
        # when comments occur later in lines.
        params <- params [which (nchar (params) > 0 &
            !grepl ("^\\s?%", params))]
        params <- paste0 (params, collapse = "\n")

        params <- tryCatch (as.list (parse (text = params)),
            error = function (e) NULL
        )

        eval_one_param_item <- function (p) {
            nm <- tryCatch (eval (p), error = function (e) NULL)
            if (is.null (nm)) {
                nm <- as.character (p)
            }
            return (unlist (nm))
        }

        nms <- lapply (params, function (i) {
            p_i <- as.list (i)
            nm <- NA_character_
            desc <- NA_integer_
            if (length (p_i) >= 3) {
                nm <- eval_one_param_item (p_i [[2]])
                desc <- eval_one_param_item (p_i [[3]])
                if (is.null (nm)) {
                    nm <- "(NULL)"
                } else if (length (nm) > 1) {
                    nm <- gsub ("\\s+", " ", paste0 (nm, collapse = ""))
                }
            }
            list (
                par_name = nm,
                nchars = sum (nchar (desc))
            )
        })

        par_name <- vapply (nms, function (i) i$par_name [1], character (1))
        nchars <- vapply (nms, function (i) i$nchars, integer (1))

        res <- data.frame (
            parameter = par_name,
            nchar = nchars,
            stringsAsFactors = FALSE
        )

        if (nrow (res) > 0) {

            res <- lapply (aliases, function (i) {
                res$alias <- i
                return (res)  })
            res <- do.call (rbind, res)
            res$doclines <- doclines
        }
    }

    if (!"doclines" %in% names (res)) {
        res$doclines <- integer (0)
    }

    return (res)
}

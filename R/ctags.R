
#' use ctags to parse call data
#'
#' @param path Path to local repository
#' @export
ctags_data <- function (path) {

    tags <- withr::with_dir (path,
                             get_ctags ())

    fns <- tags [tags$kind == "function", ]
    fn_vars <- tags [tags$kind == "functionVar", ]
    fn_vars <- fix_fnvar2fn (fn_vars, path)

    fns <- rbind (fns, fn_vars [fn_vars$kind == "function", ])
    fn_vars <- fn_vars [fn_vars$kind == "functionVar", ]

    call_graph <- fn_var_call_graph (fns, fn_vars, path)

    return (call_graph)
}

get_ctags <- function () {

    f <- tempfile (fileext = ".txt")
    cmd <- paste0 ("ctags -R --fields=eFKlnNt -f ", f, " ./R")
    system (cmd)

    # remove header lines:
    x <- readLines (f)
    x <- x [-which (grepl ("^\\!", x))]
    writeLines (x, con = f)

    suppressWarnings (
                      tags <- readr::read_delim (f,
                                                 delim = "\t",
                                                 col_names = FALSE,
                                                 col_types = readr::cols ())
                      )

    names (tags) <- c ("tag", "file", "content", "kind", "start", "language", "end")

    tags$start <- as.integer (gsub ("^line\\:", "", tags$start))
    tags$end <- as.integer (gsub ("^end\\:", "", tags$end))

    return (tags)
}

#' ctags sometimes mis-identifies R-language function calls with `kind` of
#' "functionVar". This code identifies these and manually rectifies
#' @noRd
fix_fnvar2fn <- function (fn_vars, path) {

    fn_var_tags <- unique (fn_vars$tag)

    flist <- list.files (file.path (path, "R"), full.names = TRUE)

    for (f in flist) {
        x <- utils::getParseData (parse (file = f))
        x <- x [which (!x$token == "expr"), ]

        index <- which (x$text %in% fn_var_tags) 
        j <- which (x$token [index + 1] == "LEFT_ASSIGN" &
                    x$token [index + 2] == "FUNCTION")
        new_fns <- unique (x$text [index [j]])
        index <- which (fn_vars$tag %in% new_fns)
        is_fn <- grep ("function\\s?\\(", fn_vars$content [index])
        index <- index [is_fn]
        fn_vars$kind [index] <- "function"
    }

    return (fn_vars)
}

fn_var_call_graph <- function (fns, fn_vars, path) {

    res <- NULL

    for (f in unique (fns$file)) {

        fns_f <- fns [fns$file == f, ]
        fns_f <- fns_f [order (fns_f$start), c ("tag", "start", "end")]
        fns_index <- lapply (seq (nrow (fns_f)), function (i)
                             cbind (i, seq (fns_f$start [i], fns_f$end [i])))
        fns_index <- do.call (rbind, fns_index)

        f_full <- normalizePath (file.path (path, f))
        pd <- utils::getParseData (parse (file = f_full))
        fn_calls <- pd [pd$text %in% fns$tag &
                        pd$token == "SYMBOL_FUNCTION_CALL", ]
                    index <- match (fn_calls$line1, fns_index [, 2])
                    fn_calls$fns_index <- fns_index [index, 1]
                    fn_calls$name <- fns_f$tag [fn_calls$fns_index]

                    if (nrow (fn_calls) > 0) {

                        res <- rbind (res,
                                      data.frame (file = f,
                                                  line1 = fn_calls$line1,
                                                  line2 = fn_calls$line2,
                                                  from = fn_calls$name,
                                                  to = fn_calls$text))
                    }
    }

    return (res)
}

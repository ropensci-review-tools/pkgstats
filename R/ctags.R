
#' use ctags to parse call data
#'
#' @param path Path to local repository
#' @export
ctags_data <- function (path) {

    tags <- withr::with_dir (path,
                             get_ctags ())

    fns <- tags [tags$kind == "function", ]
    fn_vars <- tags [tags$kind == "functionVar", ]

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

#' test a 'ctags' installation
#'
#' This uses the example from
#' \url{https://github.com/universal-ctags/ctags/blob/master/man/ctags-lang-r.7.rst.in}
#' and also checks the GNU global installation.
#' @export
ctags_test <- function () {

    f_in <- tempfile (fileext = ".R")

    x <- c ("G <- 1",                           # globalVar
            "v <- c(1, 2)",                     # vector
            "l <- list(3, 4)",                  # list
            "d <- data.frame(n = v)",           # dataframe + nameattr
            "f <- function(a) {",               # function
            "    g <- function (b) a + b",      # function
            "    w <- c(1, 2)",                 # vector
            "    m <- list (3, 4)",             # list
            "    e <- data.frame(n = w)",       # dataframe + nameattr
            "    L <- 2",                       # functionVar
            "}")
    writeLines (x, f_in)

    f_out <- tempfile (fileext = ".txt")
    cmd <- paste0 ("ctags --sort=no --fields=+KZ -f ", f_out, " ", f_in)
    system (cmd)

    # remove header lines:
    x <- brio::read_lines (f_out)
    x <- x [-which (grepl ("^\\!", x))]
    writeLines (x, con = f_out)

    suppressWarnings (
                      tags <- readr::read_delim (f_out,
                                                 delim = "\t",
                                                 col_names = FALSE,
                                                 col_types = readr::cols ())
                      )
    names (tags) <- c ("tag", "file", "content", "kind")

    expected_kinds <- c ("globalVar",
                         "vector",
                         "list",
                         "dataframe",
                         "nameattr",
                         "function",
                         "function",
                         "vector",
                         "list",
                         "dataframe",
                         "nameattr",
                         "functionVar")

    check <- nrow (tags) == length (expected_kinds)
    if (check)
        check <- all (tags$kind == expected_kinds)

    gtags_test <- system2 ('gtags', args = list ('--gtagslabel="new ctags"'), # nolint
                           stdout = TRUE, stderr = TRUE)
    gtags_check <- length (gtags_test) == 0L
    if (!gtags_check)
        gtags_check <- any (grepl ("error", gtags_check, ignore.case = TRUE))

    check <- check & gtags_check

    if (!check) {
        message ("ctags does not function as required; you may need to upgrade? see\n",                 # nolint
                 "https://github.com/universal-ctags/ctags/blob/master/man/ctags-lang-r.7.rst.in\n",    # nolint
                 "for expected output")
        ret <- FALSE
    } else {
        message ("ctags installation works as expected")
        ret <- TRUE
    }

    return (ret)
}

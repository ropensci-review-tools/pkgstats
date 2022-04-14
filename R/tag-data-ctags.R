#' Get tags for one directory within a package
#' @param d the directory
#' @noRd
get_ctags <- function (d = "R", has_tabs) {

    if (!dir.exists (file.path (getwd (), d))) {
        return (NULL)
    }

    path_dir <- file.path (getwd (), d)

    # tab-characters muck up parsing of tag content so have to be removed.
    # This requires modifying the code, so the whole directory is copied to
    # tempdir() and the new path returned. `path_sub` in the following is the
    # path to substitute out of file names given by ctags
    wd <- path_sub <- getwd ()
    if (has_tabs) {
        path_sub <- path_dir <- rm_tabs (path_dir)
        path_dir <- fs::path_tidy (normalizePath (file.path (path_dir, d)))
        wd <- setwd (path_dir)
        on.exit ({
            unlink (path_sub, recursive = TRUE)
            setwd (wd)
        })
        # called via withr::with_path anyway, so doesn't really matter
    }

    # ctags fields defines at
    # https://docs.ctags.io/en/latest/man/ctags.1.html#extension-fields
    # fields:
    #   - e: Line number for end of object
    #   - F: Name of source file
    #   - K: Kind of tag as long name
    #   - z: The kind key in kind field
    #   - l: language
    #   - n: Line number where `name` is defined
    #   - N: Name of language object
    #   - S: Language-specific signature of object
    #   - t: type and name of a variable

    if (d == "R") {
        fields <- "eFKlnN"
    } else if (d %in% c ("src", "inst")) {
        fields <- "eFKlnN"
    }

    ptn <- paste0 ("ctags-", Sys.getpid (), "-")
    f <- tempfile (pattern = ptn, fileext = ".txt")
    args <- c (
        "-R",
        paste0 ("--fields=", fields),
        paste0 ("-f ", f),
        path_dir
    )
    sys::exec_wait ("ctags", args, std_out = FALSE, std_err = FALSE)
    wait_for_process ("ctags")

    # remove header lines:
    x <- brio::read_lines (f)
    x <- x [-which (grepl ("^\\!", x))]

    if (length (x) == 0L) {
        chk <- rm_file_no_err (f)
        return (NULL)
    } # no ctags

    brio::write_lines (x, path = f)

    ctypes <- list (
        readr::col_character (),
        readr::col_character (),
        readr::col_character (),
        readr::col_character (),
        readr::col_character (),
        readr::col_character (),
        readr::col_character ()
    )
    cnames <- c ("tag", "file", "content", "kind", "start", "language", "end")

    n_flds <- readr::count_fields (f,
        tokenizer = readr::tokenizer_tsv (),
        n_max = 100L
    )
    if (!any (n_flds == length (cnames))) {
        chk <- rm_file_no_err (f)
        return (NULL)
    }

    suppressWarnings (
        tags <- readr::read_tsv (
            f,
            col_names = cnames,
            col_types = ctypes,
            col_select = cnames,
            progress = FALSE,
            lazy = FALSE
        )
    )

    chk <- rm_file_no_err (f)

    if (nrow (tags) == 0) {
        return (NULL)
    }

    tags <- tags [which (!grepl (excluded_file_ptn (), tags$file)), ]

    tags$start <- as.integer (gsub ("^line\\:", "", tags$start))

    # end tags may fail, and dump text other than "end:XX", so:
    index0 <- grep ("^end\\:", tags$end)
    index1 <- grep ("^[[:alpha:]]", tags$end)
    index1 <- index1 [which (!index1 %in% index0)]
    tags$end [index1] <- NA

    index <- which (!is.na (tags$end))
    tags$end [index] <- gsub ("^end\\:", "", tags$end [index])
    tags$end [index] <- gsub ("[^0-9.-]", "", tags$end [index])
    # as.integer still triggers warnings for NA values, whereas changing
    # storage.mode does not:
    storage.mode (tags$end) <- "integer"

    files <- fs::path_split (tags$file)
    len_path_sub <- length (fs::path_split (path_sub) [[1]])
    tags$file <- vapply (
        files, function (i) {
            do.call (file.path, as.list (i [-seq (len_path_sub)]))
        },
        character (1)
    )

    attr (tags, "has_tabs") <- has_tabs

    return (tags)
}

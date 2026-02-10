#' Set up gtags files if not already used
#'
#' @return `TRUE` if there are no pre-existing gtags files; otherwise `FALSE`.
#' @noRd
make_gtags <- function () {

    path <- expand_path (".")
    flist <- fs::dir_ls (
        path,
        recurse = TRUE,
        regexp = "GRTAGS$|GPATH$|GTAGS$"
    )

    if (length (flist) == 0) {
        sys::exec_wait ("gtags")
        wait_for_process ("gtags")
    }

    return (length (flist) == 0)
}

get_gtags <- function () {

    f <- fs::file_temp (pattern = "global_")
    sys::exec_wait ("global", args = c ("-rx", "."), std_out = f)
    wait_for_process ("global")

    x <- brio::read_lines (f)

    # global may fail to parse files, as happens for example with "rms" package
    if (length (x) == 0) {
        return (NULL)
    }

    # these are fixed width, but only have 4 cols, so can just replace the first
    # 3 lots of space with single tab characters.
    for (i in 1:3) {
        x <- sub (" +", "\t", x)
    }

    ctypes <- list (
        readr::col_character (),
        readr::col_double (),
        readr::col_character (),
        readr::col_character ()
    )
    cnames <- c ("tag", "line", "file", "content")
    suppressWarnings (
        gtags <- tryCatch (
            readr::read_tsv (
                paste0 (x, collapse = "\n"),
                col_names = cnames,
                col_types = ctypes,
                col_select = cnames,
                progress = FALSE,
                lazy = FALSE
            ),
            error = function (e) NULL
        )
    )

    rm_file_no_err (f)
    gtags <- rm_vendored_code (gtags)
    if (nrow (gtags) == 0L) {
        return (NULL)
    }

    gtags <- gtags [which (!grepl (excluded_file_ptn (), gtags$file)), ]

    return (gtags)
}

#' Put ctag tag objects onto the gtag references where they are called from
#' @param f one '/src' file for for which to assign ctag tags
#' @noRd
gtags_from_one_file <- function (ctags, gtags, f) {

    ctags_f <- ctags [ctags$file == f, ]
    # tags are duplicated for things like class constructors. The duplications
    # are always embedded within the main definition, so simply removing them
    # reduces line ranges to main definition only.
    ctags_f <- ctags_f [which (!duplicated (ctags_f$tag)), ]
    # end lines are not always given, as in Fortran code for which ctags works
    # but gtags does not
    if (any (is.na (ctags_f$end))) {
        return (gtags)
    }

    line_nums <- lapply (seq (nrow (ctags_f)), function (i) {
        data.frame (
            n = i,
            l = seq (
                ctags_f$start [i],
                ctags_f$end [i]
            ),
            stringsAsFactors = FALSE
        )
    })
    line_nums <- do.call (rbind, line_nums)

    index <- which (gtags$file == f)
    items <- line_nums$n [match (gtags$line [index], line_nums$l)]
    gtags$from [index] <- ctags_f$tag [items]

    return (gtags)
}

rm_gtags_files <- function (path) {

    flist <- fs::dir_ls (
        path,
        recurse = TRUE,
        regexp = "GRTAGS$|GPATH$|GTAGS$"
    )

    ret <- NULL

    if (length (flist) > 0) {

        ret <- tryCatch (fs::file_delete (flist),
            error = function (e) e
        )
    }

    return (ret)
}

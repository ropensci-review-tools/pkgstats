#' Assert and force-convert column types of 'pkgstats' summary data
#'
#' Types are defined in "inst/extdata/pkgstats-col-types.csv", with (name,
#' type) entries.
#'
#' Values which can not be converted (such as the error messages stored for
#' packages which failed to be analysed) become `NA`, but only up to a maximum
#' proportion `max_loss` of the non-missing values in each column. Greater loss
#' indicates that the dictionary does not match the data, and triggers an
#' error. The exception is "Date" columns, which retain only the date part of
#' any value, and for which any loss is accepted.
#'
#' @param x A `data.frame` of summary data, such as returned from
#' \link{pkgstats_from_archive} or \link{pkgstats_update}.
#' @param max_loss Maximum proportion of non-missing values in any column which
#' may be converted to `NA` when forcing column types.
#' @return The input `x` with all columns in the dictionary converted to their
#' specified types.
#' @noRd
force_col_types <- function (x, max_loss = 0.001) {

    stopifnot (inherits (x, "data.frame"))
    stopifnot (is.numeric (max_loss), length (max_loss) == 1L)

    dict <- read_col_types ()
    dict <- dict [dict$name %in% names (x), , drop = FALSE]

    for (i in seq_len (nrow (dict))) {

        nm <- dict$name [i]
        type <- dict$type [i]
        if (col_type_ok (x [[nm]], type)) {
            next
        }

        new <- force_one_col_type (x [[nm]], type)

        n_orig <- sum (!is.na (x [[nm]]))
        n_lost <- sum (!is.na (x [[nm]]) & is.na (new) & !is.nan (new))
        if (type != "Date" && n_orig > 0L && n_lost / n_orig > max_loss) {
            stop (
                "Forcing column '", nm, "' to '", type, "' would lose ",
                n_lost, " of ", n_orig, " non-missing values. ",
                "Please check ",
                "'inst/extdata/pkgstats-col-types.csv'.",
                call. = FALSE
            )
        }

        x [[nm]] <- new
    }

    return (x)
}

read_col_types <- function () {

    f <- system.file ("extdata", "pkgstats-col-types.csv", package = "pkgstats")
    if (!nzchar (f)) {
        stop ("Column type dictionary 'pkgstats-col-types.csv' not found.", call. = FALSE)
    }
    dict <- utils::read.csv (f, stringsAsFactors = FALSE)

    types <- c ("character", "numeric", "integer", "Date")
    stopifnot (identical (names (dict), c ("name", "type")))
    stopifnot (!anyDuplicated (dict$name))
    stopifnot (all (dict$type %in% types))

    return (dict)
}

col_type_ok <- function (v, type) {

    switch (type,
        "character" = is.character (v),
        "numeric" = is.double (v),
        "integer" = is.integer (v),
        "Date" = inherits (v, "Date")
    )
}

force_one_col_type <- function (v, type) {

    if (is.factor (v)) {
        v <- as.character (v)
    }

    switch (type,
        "character" = as.character (v),
        "numeric" = suppressWarnings (as.numeric (v)),
        "integer" = {
            num <- suppressWarnings (as.numeric (v))
            # Non-whole numbers or those out of range are lost, not truncated:
            num [which (num != round (num) | abs (num) > .Machine$integer.max)] <- NA_real_
            as.integer (num)
        },
        "Date" = force_date (v)
    )
}

force_date <- function (v) {

    if (inherits (v, "Date")) {
        return (v)
    }
    if (inherits (v, "POSIXt")) {
        return (as.Date (v, tz = "UTC"))
    }
    # Only the date part is retained, and anything else becomes `NA`:
    as.Date (substr (as.character (v), 1L, 10L), format = "%Y-%m-%d")
}

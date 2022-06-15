#' Types of 'person' to be extracted from 'Authors@R' field of 'DESCRIPTION'
#' file. See '?person' for details.
#' - aut = Author
#' - ctb = Contributor
#' - fnd = Funder
#' - rev = Reviewer
#' - ths = Thesis (Advisor)
#' - trl = Translator (usually from another language)
#' @noRd
aut_types <- function () {

    c ("aut", "ctb", "fnd", "rev", "ths", "trl")
}

#' Statistics from DESCRIPTION files
#'
#' @inheritParams loc_stats
#' @return A `data.frame` with one row and 16 columns extracting various
#' information from the 'DESCRIPTION' file, include websites, tallies of
#' different kinds of authors and contributors, and package dependencies.
#' @family stats
#' @export
#' @examples
#' f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
#' # have to extract tarball to call function on source code:
#' path <- extract_tarball (f)
#' desc_stats (path)
desc_stats <- function (path) {

    path <- check_path (path)
    if (grepl ("\\.tar\\.gz$", path)) {
        stop ("path must be directory containing package source", call. = FALSE)
    }

    desc <- file.path (path, "DESCRIPTION")
    d <- data.frame (
        read.dcf (desc),
        stringsAsFactors = FALSE
    )
    license <- d$License
    urls <- NA_character_
    if ("URL" %in% names (d)) {
        urls <- d$URL
    }

    n_aut <- desc_authors (d)

    # Dependencies:
    dep <- extract_deps (d, "Depends")
    imp <- extract_deps (d, "Imports")
    sug <- extract_deps (d, "Suggests")
    enh <- extract_deps (d, "Enhances")
    lnk <- extract_deps (d, "LinkingTo")

    bugs <- ifelse ("BugReports" %in% names (d),
        d$BugReports,
        NA_character_
    )

    desc_date <- ifelse ("Date.Publication" %in% names (d),
        gsub ("\\sUTC$", "", d$Date.Publication),
        paste0 (file.info (desc)$mtime)
    )

    data.frame (
        package = d$Package,
        version = d$Version,
        date = desc_date,
        license = license,
        urls = urls,
        bugs = bugs,
        n_aut,
        depends = dep,
        imports = imp,
        suggests = sug,
        enhances = enh,
        linking_to = lnk,
        stringsAsFactors = FALSE
    )
}

desc_authors <- function (d) {

    if ("Authors.R" %in% names (d)) {
        authors <- eval (parse (text = d$Authors.R))
        # remove everything before and after square brackets
        authors <- gsub ("^.*\\[|\\].*$", "", authors)
        n_aut <- vapply (
            aut_types (), function (i) {
                length (grep (i, authors))
            },
            integer (1)
        )
    } else {
        # There is no reliable way to establish numbers of authors for packages
        # which only have an "Author" field, because these are not intended to
        # be machine-parseable.
        authors <- d$Author
        if (grepl ("\\sand\\s", authors)) {
            authors <- strsplit (authors, "\\sand\\s") [[1]]
        } else if (grepl (",", authors)) {
            authors <- strsplit (authors, ",") [[1]]
        }
        n_aut <- rep (0, length (aut_types ()))
        names (n_aut) <- aut_types ()
        n_aut [1] <- length (authors)
    }

    n_aut <- data.frame (matrix (n_aut, nrow = 1))
    names (n_aut) <- aut_types ()

    return (n_aut)
}

extract_deps <- function (d, type = "Depends") {

    res <- d [[type]]
    res <- ifelse (length (res) == 0, NA_character_, res)
    res <- strsplit (res, ",") [[1]]

    if (type == "Depends") {
        res <- res [which (!grepl ("^R(\\s|\\()", res))]
        res <- ifelse (length (res) == 0, NA_character_, res)
    }

    res <- vapply (res, function (i) {
        strsplit (i, "\\(") [[1]] [1]
    },
    character (1),
    USE.NAMES = FALSE
    )

    res <- gsub ("^\\s+|\\s+$", "", res)

    return (paste0 (res, collapse = ", "))
}

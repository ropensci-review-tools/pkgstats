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
#' @inheritParams cloc_stats
#' @export
desc_stats <- function (path) {

    check_path (path)

    desc <- file.path (path, "DESCRIPTION")
    checkmate::assert_file (desc)

    d <- data.frame (read.dcf (desc))
    license <- d$License
    urls <- d$URL

    if ("Authors.R" %in% names (d)) {
        authors <- eval (parse (text = d$Authors.R))
        # remove everything before and after square brackets
        authors <- gsub ("^.*\\[|\\].*$", "", authors)
        n_aut <- vapply (aut_types (), function (i)
                         length (grep (i, authors)),
                         integer (1))
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

    data.frame (license = license,
                urls = urls,
                n_aut)
}

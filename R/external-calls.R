
#' Map calls from within each function to external packages
#'
#' @param path Path to package being analysed
#' @param tags_r Result of `get_ctags(dir = "R")`
#' @param pkg_name Name of package
#' @return Network of calls to external functions
#' @noRd
external_call_network <- function (tags_r, path, pkg_name) {

    # rm left objects and assignments:
    content <- gsub ("^.*(=|<\\-)", "", tags_r$content)
    content <- gsub ("function\\s*", "", content)

    # Extract function calls as alpha+.|_|: up to (\\s|\\():
    g <- gregexpr ("[[:alpha:]]([[:alpha:]]+|\\.|\\_|\\:+)*(\\s|\\()", content)
    calls <- regmatches (content, g)
    calls <- lapply (seq_along (calls), function (i)
                     if (length (calls [[i]]) > 0L)
                         data.frame (tags_line = rep (i, length (calls [[i]])),
                                     call = calls [[i]]))
    calls <- do.call (rbind, calls)

    if (length (calls) == 0L)
        return (NULL)

    calls$call <- gsub ("(\\s*\\(|\\s*)$", "", calls$call)

    calls$tag <- tags_r$tag [calls$tags_line]
    calls$kind <- tags_r$kind [calls$tags_line]
    calls$start <- tags_r$start [calls$tags_line]
    calls$end <- tags_r$end [calls$tags_line]

    calls$package <- NA_character_
    pkg_fns <- unique (tags_r$tag [tags_r$kind == "function"])
    pkg_fns <- pkg_fns [which (!grepl ("^anonFunc", pkg_fns))]
    calls$package [which (calls$call %in% pkg_fns)] <- pkg_name

    calls <- add_base_recommended_pkgs (calls)

    # Calls namespaced with `::`:
    index <- grep ("::", calls$call)
    calls$package [index] <- vapply (calls$call [index], function (i)
                                     strsplit (i, "::") [[1]] [1],
                                     character (1),
                                     USE.NAMES = FALSE)
    calls$call [index] <- gsub ("^.*::", "", calls$call [index])

    calls <- add_other_pkgs_to_calls (calls, path)

    calls <- calls [which (!is.na (calls$package)), ]

    calls$from <- tags_r$tag [calls$tags_line]
    calls$file <- tags_r$file [calls$tags_line]

    return (calls)
}

#' Use ctags to get list of fns from each pkg
#'
#' @param calls `data.frame` of calls constructed in `external_call_network`.
#' @return Input with additional column of "package" identifying packages
#' associated with each fucntion
#' @noRd
add_base_recommended_pkgs <- function (calls) {

    # namespaces of base packages are loaded, so fns can be grabbed directly
    base_pkgs <- c ("base", "stats", "graphics", "grDevices",
                    "utils", "datasets", "methods")
    for (b in base_pkgs) {
        f <- ls (paste0 ("package:", b))
        calls$package [calls$call %in% f & is.na (calls$package)] <- b
    }

    # recommended pkgs can not be (assumed to be) loaded. This list from
    # https://cran.r-project.org/src/contrib/4.1.0/Recommended/
    rcmds <- c ("KernSmooth", "MASS", "Matrix", "boot", "class", "cluster",
                "codetools", "foreign", "lattice", "mgcv", "nlme", "nnet",
                "rpart", "spatial", "survival")

    ll <- .libPaths ()

    pkg_calls <- lapply (rcmds, function (i) {

                             rpath <- file.path (ll [1], i)
                             if (!dir.exists (rpath))
                                 next

                             f <- file.path (rpath, "NAMESPACE")
                             if (!file.exists (f))
                                 next

                             n <- brio::read_lines (f)
                             fns <- parse (text = n)
                             fns <- gsub("export\\s?\\(|\\)$", "",
                                         grep ("export", fns, value = TRUE))
                             fns <- do.call (c, strsplit (fns, ","))
                             fns <- gsub ("^\\n\\s*|^\\s*", "", fns)

                             data.frame (pkg = i,
                                         fn = fns)
                })

    pkg_calls <- do.call (rbind, pkg_calls)

    na_calls <- which (is.na (calls$package))
    calls$package [na_calls] <- pkg_calls$pkg [match (calls$call [na_calls],
                                                   pkg_calls$fn [na_calls])]


    return (calls)
}

#' Add names of depends or imports packages to calls
#'
#' @inheritParams add_base_recommended_pkgs
#' @noRd
add_other_pkgs_to_calls <- function (calls, path) {

    f <- file.path (path, "NAMESPACE")
    if (!file.exists (f))
        return (calls)

    n <- parse (text = brio::read_lines (f))
    imports <- gsub ("^importFrom\\s?\\(|\\)$", "",
                 grep ("^importFrom", n, value = TRUE))

    imports <- strsplit (imports, ",")
    imports <- lapply (imports, function (i) {
                           pkg <- gsub ("\\s*\\\"\\s*", "", i [1])
                           fns <- gsub ("^\\s*|\\\"|\\s*$", "", i [-1])
                           data.frame (pkg = rep (pkg, length (fns)),
                                       fn = fns)    })
    imports <- do.call (rbind, imports)

    na_calls <- which (is.na (calls$package))
    calls$package [na_calls] <- imports$pkg [match (calls$call [na_calls],
                                                    imports$fn [na_calls])]

    return (calls)
}

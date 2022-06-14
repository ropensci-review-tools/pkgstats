
#' Map calls from within each function to external packages
#'
#' @param path Path to package being analysed
#' @param tags_r Result of `get_ctags(dir = "R")`
#' @param pkg_name Name of package
#' @return Network of calls to external functions
#' @noRd
external_call_network <- function (tags_r, path, pkg_name) {

    calls <- extract_call_content (tags_r)

    if (length (calls) == 0L) {
        return (NULL)
    }

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
    calls$package [index] <- vapply (calls$call [index], function (i) {
        strsplit (i, "::") [[1]] [1]
    },
    character (1),
    USE.NAMES = FALSE
    )
    calls$call [index] <- gsub ("^.*::", "", calls$call [index])

    calls <- add_other_pkgs_to_calls (calls, path)

    calls <- calls [which (!is.na (calls$package)), ]

    rownames (calls) <- NULL

    return (calls)
}

extract_call_content <- function (tags_r) {

    content <- gsub ("\\\"$", "", tags_r$content)
    # Remove everything within quotes - this presumes only single quotations in
    # each line, which is almost always the case.
    content <- gsub ("\\\".*\\\"", "", content)
    content <- gsub ("\\\'.*\\\'", "", content)
    index <- which (tags_r$kind == "function")

    # Remove everything within bracets of function definitions.
    br_content <- regmatches (
        content [index],
        regexpr ("\\([^\\)]+\\)", content [index])
    )
    content [index] <- vapply (
        seq_along (index), function (i) {
            gsub (br_content [i], "()",
                content [index [i]],
                fixed = TRUE
            )
        },
        character (1L)
    )

    # Then convert all symbols which are not allowed in function names to
    # spaces:
    syms <- c (
        "\\(", "\\)",
        "\\[", "\\]",
        "\\{", "\\}",
        "<\\-", "\\=",
        "\\+", "\\/", "\\*", "\\-",
        "\\|", "&", "&&",
        "\\^", "<", ">",
        ",", ";", "\\~"
    )
    content <- gsub (paste0 (syms, collapse = "|"), " ", content)

    # Then remove ...
    # isolated "$" symbols:
    content <- gsub ("^\\$|\\$$|\\s\\$\\s", "", content)
    content <- gsub ("\\b[0-9]", "", content)
    # anything after comments
    content <- gsub ("\\#.*$", "", content)
    content [which (is.na (content))] <- ""

    # Then split all around space to obtain call references
    calls <- strsplit (content, "\\s+")
    calls <- lapply (seq_along (calls), function (i) {
        if (length (calls [[i]]) > 0L) {
            cbind (
                rep (i, length (calls [[i]])),
                calls [[i]]
            )
        }
    })
    calls <- do.call (rbind, calls)
    calls <- calls [which (!calls [, 2] == ""), ]

    if (length (calls) == 0L) {
        return (NULL)
    }

    calls <- data.frame (
        tags_line = as.integer (calls [, 1]),
        call = gsub ("^[[:punct:]]+", "", calls [, 2]),
        stringsAsFactors = FALSE
    )

    rm_these <- c (
        "", "TRUE", "FALSE", "NULL",
        "NA", "...", "\\", "Inf", ":", ".",
        "function"
    )
    calls <- calls [which (!calls$call %in% rm_these), ]
    calls <- calls [which (!grepl ("\\$$|^\"|^\'", calls$call)), ]
    calls$tag <- tags_r$tag [calls$tags_line]
    calls$file <- tags_r$file [calls$tags_line]

    # rm global variables
    globals <- unique (tags_r$tag [which (tags_r$kind == "globalVar")])
    calls <- calls [which (!calls$call %in% globals), ]

    # Finally remove any functionVars (internal variables). This requires
    # matching them to the calling environment, so only those which exist in
    # each environment (function) are removed.
    fn_vars <- tags_r [which (tags_r$kind == "functionVar"), ]
    # Then make lists of fn_vars for each function
    fns <- tags_r [which (tags_r$kind == "function"), ]
    fns <- fns [which (!grepl ("^anonFunc", fns$tag)), ]

    fn_lines <- apply (
        fns [, c ("tag", "file", "start", "end")], 1,
        function (i) cbind (i [1], i [2], seq (i [3], i [4])),
        simplify = FALSE
    )

    if (is.list (fn_lines)) {

        fn_lines <- do.call (rbind, fn_lines)

    } else if (ncol (fn_lines) == 1L) {

        fn_lines <- matrix (fn_lines, ncol = 3)
    }

    fn_lines <- data.frame (
        fn_name = fn_lines [, 1],
        file = fn_lines [, 2],
        lines = as.integer (fn_lines [, 3]),
        stringsAsFactors = FALSE
    )

    for (f in unique (fn_lines$fn_name)) {

        these_lines <- fn_lines$lines [fn_lines$fn_name == f]
        this_file <- fn_lines$file [these_lines [1]]
        these_vars <- fn_vars$tag [fn_vars$file == this_file &
            fn_vars$start %in% these_lines]

        index <- which (calls$tags_line %in% these_lines &
            calls$file == this_file)
        calls$call [index] [calls$call [index] %in% these_vars] <- NA_character_
    }

    calls <- calls [which (!is.na (calls$call)), ]

    calls$call <- gsub ("(\\s*\\(|\\s*)$", "", calls$call)

    rownames (calls) <- NULL

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
    base_pkgs <- c (
        "base", "stats", "graphics", "grDevices",
        "utils", "datasets", "methods"
    )
    for (b in base_pkgs) {
        f <- ls (paste0 ("package:", b))
        calls$package [calls$call %in% f & is.na (calls$package)] <- b
    }

    # recommended pkgs can not be (assumed to be) loaded. This list from
    # https://cran.r-project.org/src/contrib/4.1.0/Recommended/
    rcmds <- c (
        "boot",
        "class",
        "cluster",
        "codetools",
        "foreign",
        "KernSmooth",
        "lattice",
        "MASS",
        "Matrix",
        "mgcv",
        "nlme",
        "nnet",
        "rpart",
        "spatial",
        "survival"
    )
    is_installed <- vapply (rcmds, function (i) {
        tryCatch (
            find.package (i),
            error = function (e) ""
        )}, character (1L))
    rcmds <- rcmds [which (nzchar (is_installed))]

    ll <- .libPaths ()

    pkg_calls <- lapply (rcmds, function (i) {

        rpath <- file.path (ll [1], i)
        if (!dir.exists (rpath)) {
            return (NULL)
        }

        f <- file.path (rpath, "NAMESPACE")
        if (!file.exists (f)) {
            return (NULL)
        }

        n <- brio::read_lines (f)
        fns <- parse (text = n)
        fns <- gsub (
            "export\\s?\\(|\\)$", "",
            grep ("export", fns, value = TRUE)
        )
        fns <- do.call (c, strsplit (fns, ","))
        fns <- gsub ("^\\n\\s*|^\\s*", "", fns)

        data.frame (
            pkg = i,
            fn = fns,
            stringsAsFactors = FALSE
        )
    })

    pkg_calls <- do.call (rbind, pkg_calls)

    if (length (pkg_calls) > 0L) {

        na_calls <- which (is.na (calls$package))
        index <- match (calls$call [na_calls], pkg_calls$fn [na_calls])
        calls$package [na_calls] <- pkg_calls$pkg [index]
    }

    return (calls)
}

#' Add names of depends or imports packages to calls
#'
#' @inheritParams add_base_recommended_pkgs
#' @noRd
add_other_pkgs_to_calls <- function (calls, path) {

    f <- file.path (path, "NAMESPACE")
    if (!file.exists (f)) {
        return (calls)
    }

    n <- parse (text = brio::read_lines (f))
    imports <- gsub (
        "^importFrom\\s?\\(|\\)$", "",
        grep ("^importFrom", n, value = TRUE)
    )

    imports <- strsplit (imports, ",")
    imports <- lapply (imports, function (i) {
        pkg <- gsub ("\\s*\\\"\\s*", "", i [1])
        fns <- gsub ("^\\s*|\\\"|\\s*$", "", i [-1])
        data.frame (
            pkg = rep (pkg, length (fns)),
            fn = fns,
            stringsAsFactors = FALSE
        )    })
    imports <- do.call (rbind, imports)

    if (!is.null (imports)) {

        na_calls <- which (is.na (calls$package))
        calls$package [na_calls] <- imports$pkg [match (
            calls$call [na_calls],
            imports$fn
        )]
    }

    # Finally, manually catch any imported package calls which are not tagged by
    # ctags - this can happen for example if calls are buried inside things like
    # `tryCatch`, or if they don't form full expressions. This relies on
    # getParseData, which is less reliable than ctags, but can catch some
    # outliers.
    pkgs_not_called <- imports$pkg [which (!imports$pkg %in% calls$package)]
    if (length (pkgs_not_called) == 0L) {
        return (calls)
    }

    imports_not_called <- imports [imports$pkg %in% pkgs_not_called, ]

    r_files <- normalizePath (list.files (
        file.path (path, "R"),
        full.names = TRUE,
        pattern = "\\.(r|R|q|s|S)$"
    ))
    all_calls <- paste0 (imports_not_called$fn, collapse = "|")
    tokens <- c ("FUNCTION", "SYMBOL_FUNCTION_CALL", "SPECIAL")

    external_calls <- lapply (r_files, function (i) {

        pd <- utils::getParseData (control_parse (i))
        pd <- pd [grep (all_calls, pd$text), ]
        pd <- pd [which (pd$token %in% tokens), ]

        if (nrow (pd) == 0L) {
            return (NULL)
        }

        data.frame (
            tags_line = pd$text,
            call = pd$text,
            tag = pd$token,
            file = paste0 ("R/", basename (i)),
            kind = "unknown",
            start = pd$line1,
            end = pd$line2
        )
    })
    external_calls <- do.call (rbind, external_calls)

    if (is.null (external_calls)) {
        return (calls)
    }

    if (nrow (external_calls) == 0L) {
        return (calls)
    }

    external_calls$package <-
        imports_not_called$pkg [match (
            external_calls$call,
            imports_not_called$fn
        )]

    external_calls$tags_line <- seq (nrow (external_calls)) +
        max (calls$tags_line, na.rm = TRUE)

    calls <- rbind (calls, external_calls)

    return (calls)
}

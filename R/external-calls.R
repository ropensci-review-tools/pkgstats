
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

    calls$file <- tags_r$file [calls$tags_line]

    rownames (calls) <- NULL

    return (calls)
}

extract_call_content <- function (tags_r) {

    content <- gsub ("\\\"$", "", tags_r$content)
    # Remove everything within quotes - this presumes only single quotations in
    # each line, which is almost always the case.
    content <- gsub ("\\\".*\\\"", "", content)
    # Remove everything within bracets of function definitions
    index <- which (tags_r$kind == "function")

    bracket_content <- regmatches (
        content [index],
        gregexpr ("(?<=\\().*?(?=\\))",
            content [index],
            perl = T
        )
    )
    bracket_content <- vapply (
        bracket_content, function (i) {
              ifelse (length (i) == 0L,
                  "",
                  i [1]
              )
          },
        character (1)
    )

    has_content <- which (nchar (bracket_content) > 0L)
    bracket_content <- bracket_content [has_content]
    index <- index [has_content]

    content [index] <- vapply (
        seq_along (index), function (i) {
              content [index [i]] <-
                  gsub (bracket_content [i], "",
                      content [index [i]],
                      fixed = TRUE
                  )
          },
        character (1)
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
        call = calls [, 2]
    )

    rm_these <- c (
        "TRUE", "FALSE", "NULL",
        "NA", "...", "\\", "Inf", ":", ".",
        "function"
    )
    calls <- calls [which (!calls$call %in% rm_these), ]
    calls <- calls [which (!grepl ("\\$$|^\"|^\'", calls$call)), ]
    calls$call <- gsub ("^\\!", "", calls$call)
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
        function (i) cbind (i [1], i [2], seq (i [3], i [4]))
    )

    if (is.list (fn_lines)) {

        fn_lines <- do.call (rbind, fn_lines)

    } else if (ncol (fn_lines) == 1L) {

        fn_lines <- matrix (fn_lines, ncol = 3)
    }

    fn_lines <- data.frame (
        fn_name = fn_lines [, 1],
        file = fn_lines [, 2],
        lines = as.integer (fn_lines [, 3])
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
        "KernSmooth", "MASS", "Matrix", "boot", "class", "cluster",
        "codetools", "foreign", "lattice", "mgcv", "nlme", "nnet",
        "rpart", "spatial", "survival"
    )
    ip <- data.frame (utils::installed.packages ())
    rcmds <- rcmds [which (rcmds %in% ip$Package)]

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
            fn = fns
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
            fn = fns
        )    })
    imports <- do.call (rbind, imports)

    if (!is.null (imports)) {

        na_calls <- which (is.na (calls$package))
        calls$package [na_calls] <- imports$pkg [match (
            calls$call [na_calls],
            imports$fn [na_calls]
        )]
    }

    return (calls)
}

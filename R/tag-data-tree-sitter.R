#' Get tags using tree-sitter
#'
#' Currently extracts tags from the "R" directory only, and only for R-language files.
#'
#' @inheritParams tags_data
#' @noRd
get_treesitter_tags <- function (path) {

    language <- treesitter.r::language ()
    parser <- treesitter::parser (language)

    tree_sitter_calls (parser, path)
}

tree_sitter_calls <- function (parser, path, d = "R") {
    flist <- fs::dir_ls (fs::path (path, d), pattern = "\\.R$")
    fn_calls <- lapply (flist, function (f) {
        parse_list <- control_parse (f)
        fn_calls <- lapply (parse_list, function (p) {
            txt <- paste0 (as.character (p), collapse = "\n")
            tree <- treesitter::parser_parse (parser, txt)
            walk_one_tree (tree)
        })
        res <- do.call (rbind, fn_calls)
        cbind (file = rep (f, nrow (res)), res)
    })
    fn_calls <- do.call (rbind, fn_calls)
}

walk_one_tree <- function (tree) {

    it <- treesitter::tree_walk (tree)

    reached_foot <- FALSE
    first_identifier <- TRUE
    get_next_open <- FALSE

    grammar_types <- node_text <- next_open <- fn_name <- character (0L)

    while (!reached_foot) {
        field_name <- NA_to_char (it$field_name ())
        grammar_type <- NA_to_char (treesitter::node_grammar_type (it$node ()))
        if (field_name == "function" && !grammar_type %in% c ("call", "extract_operator")) {
            grammar_types <- c (grammar_types, grammar_type)
            node_text <- c (node_text, treesitter::node_text (it$node ()))
            get_next_open <- TRUE
        } else if (grammar_type == "identifier" && first_identifier) {
            fn_name <- treesitter::node_text (it$node ())
            first_identifier <- FALSE
        } else if (get_next_open && field_name == "open") {
            next_open <- c (next_open, grammar_type)
            get_next_open <- FALSE
        }

        if (it$goto_first_child ()) next
        if (it$goto_next_sibling ()) next

        retracing <- TRUE
        while (retracing) {
            if (!it$goto_parent ()) {
                retracing <- FALSE
                reached_foot <- TRUE
            }
            if (it$goto_next_sibling ()) {
                retracing <- FALSE
            }
        }
    }

    # This line ensures fn_name is also length 0 when no data are parsed:
    fn_name <- rep (fn_name, length (grammar_types))
    data.frame (
        fn_name = fn_name,
        grammar_type = grammar_types,
        node_text = node_text
    ) [which (next_open != "["), ]
}


# content mostly from srr::srr_stats_pkg_skeleton

make_pkg_path <- function (base_dir = tempdir (), pkg_name = "demo") {

    d <- file.path (base_dir, pkg_name)
    if (!file.exists (d))
        dir.create (d, recursive = TRUE)

    return (d)
}

write_desc <- function (d, pkg_name) {

    desc <- c (paste0 ("Package: ", pkg_name),
               "Title: What the Package Does (One Line, Title Case)",
               "Version: 0.0.0.9000",
               "Authors@R: ",
               "  person(given = \"First\",",
               "         family = \"Last\",",
               "         role = c(\"aut\", \"cre\"),",
               "         email = \"first.last@example.com\")",
               "Description: What the package does (one paragraph).",
               "License: GPL-3",
               "Encoding: UTF-8")

    writeLines (desc, con = file.path (d, "DESCRIPTION"))
}

write_r_fn <- function (d, pkg_name) {

    rfile <- c ("#' test_fn",
                "#'",
                "#' A test funtion",
                "#'",
                "#' @param a A number",
                "#' @param b A number",
                "#' @export",
                "test_fn <- function(a, b) {",
                "    if (!is.numeric (a)) stop ('nope')",
                "    if (length (a) > 1) a <- a [1]",
                "    if (!is.numeric (b)) stop ('nope')",
                "    if (length (b) > 1) b <- b [1]",
                "    y <- runif (a, 0, b)",
                "    return (y)",
                "}",
                "",
                "#' Another fn",
                "#'",
                "#' @param x A number",
                "#' @export",
                "test_fn2 <- function (x) {",
                "    x <- sqrt (abs (x))",
                "    y <- test_fn (5, 2)",
                "    return (x * y)",
                "}")
    dr <- file.path (d, "R")
    if (!file.exists (dr))
        dir.create (dr)
    writeLines (rfile, con = file.path (dr, "test.R"))

    rfile <- c ("#' @keywords internal",
                "\"_PACKAGE\"",
                "",
                paste0 ("# The following block is used by ",
                        "usethis to automatically manage"),
                "# roxygen namespace tags. Modify with care!",
                "## usethis namespace: start",
                "## usethis namespace: end",
                "NULL")
    writeLines (rfile, con = file.path (dr, paste0 (pkg_name, "-package.R")))
}

make_demo_package <- function () {

    pkg_name <- paste0 (sample (c (letters, LETTERS), 8), collapse = "")
    #path <- make_pkg_path (tempdir (), pkg_name)
    path <- make_pkg_path ()

    write_desc (path, pkg_name)
    write_r_fn (path, pkg_name)

    roxygen2::roxygenise (path)

    pkgbuild::build (path)
}

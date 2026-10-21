test_that ("extract_call_content handles non-UTF-8 content", {

    # 0x80 is invalid as a standalone UTF-8 byte; in a UTF-8 locale gsub()
    # would throw "invalid multibyte string" without the iconv() guard.
    bad_content <- paste0 ("my_fn <- function(x) { paste(x) \x80 }")

    tags_r <- data.frame (
        tag = "my_fn",
        file = "R/test.R",
        content = bad_content,
        kind = "function",
        start = 1L,
        end = 3L,
        stringsAsFactors = FALSE
    )

    expect_no_error (pkgstats:::extract_call_content (tags_r))
})

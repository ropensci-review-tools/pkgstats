fake_summary <- function (pkg, files_R = 3L, rel_space = 0.25) {
    s <- pkgstats_summary ()
    s$package <- pkg
    s$version <- "1.0.0"
    s$date <- "2026-10-09 12:00:00"
    s$files_R <- files_R
    s$rel_space_R <- rel_space
    return (s)
}

test_that ("rbind_summaries drops failed results", {

    res <- list (
        fake_summary ("a"),
        NULL,
        structure ("Error : [ENOENT] Failed to remove\n", class = "try-error"),
        fake_summary ("b")
    )
    expect_warning (
        x <- rbind_summaries (res),
        "Dropping 1 failed result"
    )
    expect_equal (nrow (x), 2L)
    expect_type (x$files_R, "integer")
    expect_type (x$rel_space_R, "double")

    expect_silent (x <- rbind_summaries (list (fake_summary ("a"), NULL)))
    expect_equal (nrow (x), 1L)
})

test_that ("restore_column_types restores numeric columns", {

    x <- rbind (fake_summary ("a"), fake_summary ("b", 10L, 0.125))
    expect_true (check_column_types (x))

    # Binding a character string coerces every column, as in the data
    # uploaded on 2026-10-09:
    err <- "Error : [ENOENT] Failed to remove\n"
    xc <- rbind (x, err)
    expect_true (all (vapply (xc, is.character, logical (1L))))
    expect_error (check_column_types (xc), "should be numeric")

    y <- restore_column_types (xc)
    expect_equal (nrow (y), 2L)
    expect_true (check_column_types (y))
    expect_equal (y$files_R, c (3L, 10L))
    expect_equal (y$rel_space_R, c (0.25, 0.125))
    expect_type (y$package, "character")
    expect_s3_class (y$date, "POSIXct")

    xc$files_R [1] <- "three"
    expect_error (restore_column_types (xc), "not numbers")
})

test_that ("force_col_types", {

    dict <- read_col_types ()
    expect_equal (names (dict), c ("name", "type"))
    expect_true (all (dict$type %in% c ("character", "numeric", "integer", "Date")))
    expect_identical (dict$name, names (null_stats ()))

    x <- null_stats ()
    x [1, ] <- NA_character_
    x [] <- lapply (x, as.character)
    x$package <- "a"
    x$date <- "2024-08-16 09:10:05"
    x$loc_R <- "12"
    x$rel_space <- "0.5"
    x$nexpr <- "NaN"

    y <- force_col_types (x)
    expect_type (y$loc_R, "integer")
    expect_type (y$rel_space, "double")
    expect_s3_class (y$date, "Date")
    expect_equal (y$date, as.Date ("2024-08-16"))
    expect_type (y$package, "character")
    expect_equal (y$loc_R, 12L)
    expect_identical (force_col_types (y), y)

    # Non-whole values can not be integers:
    x$loc_R <- "12.5"
    expect_error (force_col_types (x), "would lose")
    # but isolated failures are tolerated up to 'max_loss':
    x <- x [rep (1, 2000), ]
    x$loc_R <- "1"
    x$loc_R [1] <- "Error : something failed"
    expect_silent (force_col_types (x))
    expect_error (force_col_types (x, max_loss = 0), "would lose")
})

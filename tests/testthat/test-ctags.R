test_all <- (identical (Sys.getenv ("RRT_TEST_ALL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

skip_if (!test_all)

test_that ("ctags-test", {

    expect_message (
        x <- ctags_test (quiet = FALSE),
        "ctags installation works as expected"
    )

    expect_true (x)
})

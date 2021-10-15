
test_that ("ctags-test", {

    expect_message (
        x <- ctags_test (),
        "ctags installation works as expected"
    )

    expect_true (x)
})

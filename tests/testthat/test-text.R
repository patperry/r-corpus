context("text")


test_that("`format` should handle NAs", {
    x <- c(NA, "Friday, November 23, 1787", NA)
    expect_equal(format(as.text(x)), format(as.character(x)))
})


test_that("`as.data.frame` should work", {
    x <- text(a="1", b="2", c="foo", d="bar")
    d <- as.data.frame(x)

    expect_equal(nrow(d), length(x))
    expect_equal(names(d), "x")
    expect_equal(rownames(d), names(x))
    expect_equal(d[["x"]], as.text(x)) # drop names
})

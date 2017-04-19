context("text_format")


test_that("`format` should handle NAs", {
    x <- c(NA, "Friday, November 23, 1787", NA)
    expect_equal(format(as.text(x)), format(as.character(x)))
})

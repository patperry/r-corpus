context("utf8_width")

test_that("'utf8_width' computes widths correctly", {
    expect_equal(utf8_width(c("hello", "\u200b", "\u22ee", "\u6027",
                              intToUtf8(0x1f642))),
                 c(5, 0, 1, 2, 2))
})


test_that("'utf8_width' keeps names", {
    expect_equal(utf8_width(c(a = "hello", b = "you")),
                 c(a = 5, b = 3))
})


test_that("'utf8_width' gives NA for invalid data", {
    x <- c("a", "b", "\xff", "abc\xfe")
    Encoding(x) <- "UTF-8"
    expect_equal(utf8_width(x), c(1, 1, NA, NA))
})

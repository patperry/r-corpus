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

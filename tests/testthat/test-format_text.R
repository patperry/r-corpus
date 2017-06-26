context("format.text")


test_that("'format' can handle short text", {
    raw <- c("a", "foo", "short text")
    text <- as_text(raw)

    expect_equal(format(text, justify = "none"),
                 format(raw, justify = "none"))

    expect_equal(format(text, justify = "left"),
                 format(raw, justify = "left"))

    expect_equal(format(text, justify = "right"),
                 format(raw, justify = "right"))
})

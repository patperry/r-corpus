context("text_sub")


test_that("'text_sub' errors for invalid 'start' arg", {
    x <- as_text(c("A man, a plan.", "A \"canal\"?", "Panama!"),
                 filter = text_filter(drop_punct = TRUE))
              
    expect_error(text_sub(x, c(1, 2)),
                 "'start' length does not evenly divide argument length")

    expect_error(text_sub(x, c(1, 2, 3, 4)),
                 "'start' length does not evenly divide argument length")

    expect_error(text_sub(x, integer()),
                 "'start' length does not evenly divide argument length")

    expect_error(text_sub(x, "1"),
                 "'start' must be an integer vector or two-column matrix")

    expect_error(text_sub(x, matrix(1:3, 1, 3)),
                 "'start' must be an integer vector or two-column matrix")
})


test_that("'text_sub' errors for invalid 'end' arg", {
    x <- as_text(c("A man, a plan.", "A \"canal\"?", "Panama!"),
                 filter = text_filter(drop_punct = TRUE))
              
    expect_error(text_sub(x, 1, c(1, 2)),
                 "'end' length does not evenly divide argument length")

    expect_error(text_sub(x, 1, c(1, 2, 3, 4)),
                 "'end' length does not evenly divide argument length")

    expect_error(text_sub(x, 1, integer()),
                 "'end' length does not evenly divide argument length")

    expect_error(text_sub(x, 1, "1"),
                 "'end' must be an integer vector")

    expect_error(text_sub(x, end = matrix(1:2, 1, 2)),
                 "'end' must be an integer vector")

    expect_warning(text_sub(x, start = matrix(1, 1, 2), end = 1),
        "'end' argument is ignored when 'start' is a two-column matrix")
})

context("frame-stats")


test_that("'na.fail' works", {
    data <- corpus_frame(x = 26:1, text = letters)
    expect_equal(na.fail(data), data)

    data <- corpus_frame(x = c(NA, 26:2), text = letters)
    expect_error(na.fail(data), "missing values in object")

    data <- corpus_frame(x = 1:26, text = c(NA, letters[-1]))
    expect_error(na.fail(data), "missing values in object")
})


test_that("'na.omit' works", {
    data <- corpus_frame(text = c(NA, "a", "b", NA, "c"), x = 2:6)
    actual <- na.omit(data)
    expected <- corpus_frame(text = c("a", "b", "c"), x = c(3, 4, 6),
                             row.names = c(2L, 3L, 5L))
    omit <- c("1" = 1L, "4" = 4L)
    attr(omit, "class") <- "omit"
    attr(expected, "na.action") <- omit
    expect_equal(actual, expected)

    expect_equal(na.omit(corpus_frame(x = 1:26, text = letters)),
                 corpus_frame(x = 1:26, text = letters))
})


test_that("'na.exclude' works", {
    data <- corpus_frame(text = letters[1:5], x = c(4, 3, NA, 1, -7),
                         row.names = c("A", "B", "C", "D", "E"))
    actual <- na.exclude(data)
    expected <- corpus_frame(text = c("a", "b", "d", "e"),
                              x = c(4, 3, 1, -7),
                              row.names = c("A", "B", "D", "E"))
    exclude <- c("C" = 3L)
    attr(exclude, "class") <- "exclude"
    attr(expected, "na.action") <- exclude
    expect_equal(actual, expected)

    expect_equal(na.exclude(corpus_frame(x = 1:26, text = letters)),
                 corpus_frame(x = 1:26, text = letters))
})

context("text-stats")

test_that("'na.fail' works", {
    x <- as_corpus_text(letters)
    expect_equal(na.fail(x), x)

    expect_error(na.fail(c(x, NA)), "missing values in object")
})


test_that("'na.omit' works", {
    x <- as_corpus_text(c(NA, "a", "b", NA, "c"))
    actual <- na.omit(x)
    expected <- as_corpus_text(c("a", "b", "c"))
    omit <- c(1L, 4L)
    attr(omit, "class") <- "omit"
    attr(expected, "na.action") <- omit
    expect_equal(actual, expected)

    expect_equal(na.omit(as_corpus_text(letters)),
                 as_corpus_text(letters))
})


test_that("'na.exclude' works", {
    x <- as_corpus_text(c(r = NA, s = "a", t = "b", u = NA, v = "c"))
    actual <- na.exclude(x)
    expected <- as_corpus_text(c(s = "a", t = "b", v = "c"))
    exclude <- c(r = 1L, u = 4L)
    attr(exclude, "class") <- "exclude"
    attr(expected, "na.action") <- exclude
    expect_equal(actual, expected)

    expect_equal(na.exclude(as_corpus_text(letters)),
                 as_corpus_text(letters))
})

context("utf8")

test_that("'utf8_valid' errors on latin1 declared to be UTF-8", {
    x <- c("a", "b", "the command of her beauty, and her \xa320,000", "d")
    Encoding(x) <- "UTF-8"

    msg <- paste("entry 3 is marked as \"UTF-8\" but string byte 36",
                 "(\"\\xa3\") is invalid in that encoding")
    expect_equal(utf8_valid(x), msg)
})


test_that("utf8_valid errors on invalid UTF-8", {
    x <- c("a", "b", "c", "d", "\U00200000")
    Encoding(x) <- "UTF-8"

    msg <- paste("entry 5 is marked as \"UTF-8\"",
                 "but string byte 1 (\"\\xf8\") is invalid in that encoding")
    expect_equal(utf8_valid(x), msg)
})


test_that("utf8_valid passes on valid UTF-8 in bytes encoding", {
    x <- "hello\u2002"
    Encoding(x) <- "bytes"
    expect_true(utf8_valid(x))
})


test_that("utf8_valid passes on valid ASCII in unknown encoding", {
    x <- "world"
    expect_true(utf8_valid(x))
})


test_that("utf8_valid errors on invalid UTF8 in bytes encoding", {
    x <- "hello\Ufffffff"
    Encoding(x) <- "bytes"
    msg <- paste("cannot convert entry 1 from \"bytes\" encoding",
                 "to \"UTF-8\"; string byte 6 (\"\\xfc\") is invalid")
    expect_equal(utf8_valid(x), msg)
})

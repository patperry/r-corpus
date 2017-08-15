context("text_stats")


test_that("'text_stats' works on a simple example", {
    x <- c("A rose is a rose is a rose.", "A Rose is red. A violet is blue!")
    actual <- text_stats(x)
    expected <- data.frame(tokens = text_ntoken(x),
                           types = text_ntype(x),
                           sentences = text_nsentence(x))
    class(expected) <- c("corpus_frame", "data.frame")
    expect_equal(actual, expected)
})

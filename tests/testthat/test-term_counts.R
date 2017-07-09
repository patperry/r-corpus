context("term_counts")

test_that("'term_counts' works", {
    expect_equal(term_counts("A rose is a rose is a rose."),
                 structure(data.frame(term = c("a", "rose", "is", "."),
                                      count = c(3, 3, 2, 1),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})


test_that("'term_counts' can use a filter", {
    f <- token_filter(drop_punct = TRUE, drop = stopwords("english"))
    expect_equal(term_counts("A rose is a rose is a rose.", f),
                 structure(data.frame(term = c("rose"),
                                      count = c(3),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})


test_that("'term_counts' can use weights", {
    x <- c("A rose is a rose is a rose.",
           "A Rose is red, a violet is blue!")
    weights <- c(100, 1)

    expect_equal(term_counts(x, weights = weights),
                 structure(data.frame(term = c("a", "rose", "is", ".",
                                               "!", ",", "blue", "red",
                                               "violet"),
                                      count = c(302, 301, 202, 100, 1,
                                                1, 1, 1, 1),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})


test_that("'term_counts' can use a filter and weights", {
    x <- c("A rose is a rose is a rose.",
           "A Rose is red, a violet is blue!")
    f <- token_filter(drop_punct = TRUE, drop = stopwords("english"))
    weights <- c(100, 1)

    expect_equal(term_counts(x, f, weights = weights),
                 structure(data.frame(term = c("rose", "blue", "red", "violet"),
                                      count = c(301, 1, 1, 1),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})


test_that("'term_counts' can count ngrams", {
    expect_equal(term_counts("A rose is a rose is a rose.", ngrams = 2),
                 structure(data.frame(term = c("a rose", "is a", "rose is",
                                               "rose ."),
                                      count = c(3, 2, 2, 1),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})


test_that("'term_counts' can count ngrams above min", {
    expect_equal(term_counts("A rose is a rose is a rose.", ngrams = 2,
                             min_count = 2),
                 structure(data.frame(term = c("a rose", "is a", "rose is"),
                                      count = c(3, 2, 2),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})

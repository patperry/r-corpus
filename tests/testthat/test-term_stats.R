context("term_stats")

test_that("'term_stats' works", {
    expect_equal(term_stats("A rose is a rose is a rose."),
                 structure(data.frame(term = c("a", "rose", "is", "."),
                                      count = c(3, 3, 2, 1),
                                      support = c(1, 1, 1, 1),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})


test_that("'term_stats' can use a filter", {
    f <- text_filter(drop_punct = TRUE, drop = stopwords("english"))
    expect_equal(term_stats("A rose is a rose is a rose.", f),
                 structure(data.frame(term = c("rose"),
                                      count = c(3),
                                      support = c(1),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})


test_that("'term_stats' can use weights", {
    x <- c("A rose is a rose is a rose.",
           "A Rose is red, a violet is blue!")
    weights <- c(100, 1)

    term <- c("a", "rose", "is", ".", "!", ",", "blue", "red", "violet")
    count <- c(302, 301, 202, 100, 1, 1, 1, 1, 1)
    support <- c(101, 101, 101, 100, 1, 1, 1, 1, 1)
    o <- order(-support, -count, term)

    expect_equal(term_stats(x, weights = weights),
                 structure(data.frame(term = term[o], count = count[o],
                                      support = support[o],
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})


test_that("'term_stats' can use a filter and weights", {
    x <- c("A rose is a rose is a rose.",
           "A Rose is red, a violet is blue!")
    f <- text_filter(drop_punct = TRUE, drop = stopwords("english"))
    weights <- c(100, 1)

    expect_equal(term_stats(x, f, weights = weights),
                 structure(data.frame(term = c("rose", "blue", "red", "violet"),
                                      count = c(301, 1, 1, 1),
                                      support = c(101, 1, 1, 1),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})


test_that("'term_stats' can count ngrams", {
    expect_equal(term_stats("A rose is a rose is a rose.", ngrams = 2),
                 structure(data.frame(term = c("a rose", "is a", "rose is",
                                               "rose ."),
                                      count = c(3, 2, 2, 1),
                                      support = c(1, 1, 1, 1),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})


test_that("'term_stats' can count ngrams above count_min", {
    expect_equal(term_stats("A rose is a rose is a rose.", ngrams = 2,
                             min_count = 2),
                 structure(data.frame(term = c("a rose", "is a", "rose is"),
                                      count = c(3, 2, 2),
                                      support = c(1, 1, 1),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})


test_that("'term_stats' can count ngrams above support_min", {
    expect_equal(term_stats(c("A rose is a rose is a rose.", "Rose Red"),
                             ngrams = 1,
                             min_support = 2),
                 structure(data.frame(term = c("rose"),
                                      count = c(4),
                                      support = c(2),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})


test_that("'term_stats' can output types", {
    expect_equal(term_stats("A rose is a rose is a rose.", ngrams = 2,
                             min_count = 2, types = TRUE),
                 structure(data.frame(term = c("a rose", "is a", "rose is"),
                                      type1 = c("a", "is", "rose"),
                                      type2 = c("rose", "a", "is"),
                                      count = c(3, 2, 2),
                                      support = c(1, 1, 1),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})

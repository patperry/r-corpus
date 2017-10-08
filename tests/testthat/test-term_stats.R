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
    f <- text_filter(drop_punct = TRUE, drop = stopwords_en)
    expect_equal(term_stats("A rose is a rose is a rose.", f),
                 structure(data.frame(term = c("rose"),
                                      count = c(3),
                                      support = c(1),
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


test_that("'term_stats' can select terms", {
    expect_equal(term_stats("A rose is a rose is a rose.",
                            subset = term %in% c("rose", "a")),
                 structure(data.frame(term = c("a", "rose"),
                                      count = c(3, 3),
                                      support = c(1, 1),
                                      stringsAsFactors = FALSE),
                           class = c("corpus_frame", "data.frame")))
})


test_that("'term_stats' errors for invalid 'subset' argument", {
    expect_error(term_stats("A rose is a rose is a rose.", subset = "rose"),
                 "'subset' must be logical")
})


test_that("'term_stats' errors for invalid 'count', 'support' arguments", {
    expect_error(term_stats("hello", min_count = c(1, 2)),
                 "'min_count' must have length 1")
    expect_error(term_stats("hello", max_count = NA),
                 "'max_count' must be a numeric value (or NULL)", fixed = TRUE)
})


test_that("'term_stats' errors for invalid 'ngrams' argument", {
    expect_error(term_stats("hello", ngrams = "1"),
                 "'ngrams' must be NULL or an integer vector")
    expect_error(term_stats("hello", ngrams = c(NA, 1)),
                 "'ngrams' entries must be positive integer values")
    expect_error(term_stats("hello", ngrams = c(1, 0)),
                 "'ngrams' entries must be positive integer values")
    expect_error(term_stats("hello", ngrams = 128),
                 "'ngrams' entries must be below 128")
    expect_error(term_stats("hello", ngrams = integer()),
                 "'ngrams' argument cannot have length 0")
})

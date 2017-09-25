context("stemmer.R")

test_that("can use a custom function as a stemmer", {
    x <- LETTERS
    stem <- function(word) "?"
    expect_equal(text_tokens(x, stemmer = stem),
                 as.list(rep("?", length(x))))
})


test_that("handles stemmer logical NAs", {
    x <- paste(LETTERS, collapse = " ")
    stemmer <-
        function(x) {
            if (x %in% c("a", "e", "i", "o", "u")) {
                paste0(toupper(x), "*")
            } else {
                NA
            }
        }
    actual <- text_tokens(x, stemmer = stemmer)
    expected <- list(c("A*", "E*", "I*", "O*", "U*"))
    expect_equal(actual, expected)
})


test_that("handles stemmer character NAs", {
    x <- paste(LETTERS, collapse = " ")
    stemmer <- function(x) NA_character_
    actual <- text_tokens(x, stemmer = stemmer)
    expected <- list(character())
    expect_equal(actual, expected)
})


test_that("handles stemmer errors", {
    x <- LETTERS

    expect_error(text_tokens(x, stemmer = function(w) c("?", "?")),
                 "'stemmer' returned multiple values for input \"a\"")

    expect_error(text_tokens(x, stemmer = function(w) character()),
                 "'stemmer' did not return a value for input \"a\"")

    expect_error(text_tokens(x, stemmer = function(w) NULL),
                 "'stemmer' did not return a value for input \"a\"")

    expect_error(text_tokens(x, stemmer = function(w) 7),
                 "'stemmer' returned a non-string value for input \"a\"")
})


test_that("handles internal stemmer errors", {
    expect_error(text_tokens("hello", stemmer = function(x) stop("what?")),
                 "'stemmer' raised an error for input \"hello\"")
})

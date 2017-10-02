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


test_that("'new_stemmer' can detect errors", {
    expect_error(new_stemmer(c("a", "b"), c("a")),
                 "'term' argument length must equal 'stem' argument length")
})


test_that("'new_stemmer' can handle empty inputs", {
    fn <- new_stemmer(NULL, NULL)
    expect_equal(fn("a"), "a")
})


test_that("'new_stemmer' can use a default", {
    fn <- new_stemmer(LETTERS, letters, default = NA)
    expect_equal(fn("A"), "a")
    expect_equal(fn("AB"), NA_character_)
})


test_that("'new_stemmer' can handle duplicates", {
    term <- c("a", "a", "b", "c", "c", "c", "d")
    stem <- c("a1", "a2", "b", "c1", "c2", "c3", "d")

    fn <- new_stemmer(term, stem, duplicates = "first", vectorize = FALSE)
    expect_equal(sapply(term, fn, USE.NAMES = FALSE),
                 c("a1", "a1", "b", "c1", "c1", "c1", "d"))

    fn <- new_stemmer(term, stem, duplicates = "last", vectorize = FALSE)
    expect_equal(sapply(term, fn, USE.NAMES = FALSE),
                 c("a2", "a2", "b", "c3", "c3", "c3", "d"))

    fn <- new_stemmer(term, stem, duplicates = "omit", vectorize = FALSE)
    expect_equal(sapply(term, fn, USE.NAMES = FALSE),
                 c("a", "a", "b", "c", "c", "c", "d"))
})


test_that("'new_stemmer' can vectorize ", {
    term <- c("a", "a", "b", "c", "c", "c", "d")
    stem <- c("a1", "a2", "b", "c1", "c2", "c3", "d")

    fn <- new_stemmer(term, stem, duplicates = "first", vectorize = TRUE)
    expect_equal(fn(term), c("a1", "a1", "b", "c1", "c1", "c1", "d"))

    fn <- new_stemmer(term, stem, duplicates = "last", vectorize = TRUE)
    expect_equal(fn(term), c("a2", "a2", "b", "c3", "c3", "c3", "d"))

    fn <- new_stemmer(term, stem, duplicates = "omit", vectorize = TRUE)
    expect_equal(fn(term), c("a", "a", "b", "c", "c", "c", "d"))

    expect_error(new_stemmer(term, stem, duplicates = "fail",
                              vectorize = TRUE),
                 "'term' argument entries must be unique")
})


test_that("'stem_snowball' can handle NULL algorithm", {
    x <- c("win", "winning", "winner", "#winning")
    expect_equal(stem_snowball(x, NULL), x)
})


test_that("'stem_snowball' can handle NULL input", {
    expect_equal(stem_snowball(NULL), NULL)
})


test_that("'stem_snowball' can handle stem input", {
    x <- c("win", "winning", "winner", "#winning")
    expect_equal(stem_snowball(x),
                 c("win", "win", "winner", "#winning"))
})

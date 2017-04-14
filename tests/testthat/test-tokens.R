context("tokens")


test_that("'tokens' splits according to UAX #29", {
    text <- paste0("The quick (\u201cbrown\u201d) fox can\u2019t",
                   " jump 32.3 feet, right?")

    toks <- tokens(text, filter=NULL)

    expect_equal(toks, list(
        c("The", " ", "quick", " ", "(", "\u201c", "brown", "\u201d", ")",
         " ", "fox", " ", "can\u2019t", " ", "jump", " ", "32.3", " ",
         "feet", ",", " ", "right", "?")))
})


test_that("'tokens' normalizes token by default", {
    text <- paste0("The quick (\u201cbrown\u201d) fox can\u2019t",
                   " jump 32.3 feet, right?")

    toks <- tokens(text)

    expect_equal(toks, list(
        c("the", "quick", "(", "'", "brown", "'", ")", "fox", "can't",
          "jump", "32.3", "feet", ",", "right", "?")))
})


test_that("'tokens' propagates names if its argument has them", {
    text <- c(a="First sentence.", b="Second sentence!")

    toks <- tokens(text)

    expect_equal(toks, list(
        a=c("first", "sentence", "."),
        b=c("second", "sentence", "!")))
})


test_that("'tokens' works on empty values", {
    expect_equal(tokens(c()), list())
})


test_that("'tokens' works on empty and missing values", {
    expect_equal(tokens(c("1", "2", "", NA, "5")),
                 list("1", "2", character(), NA_character_, "5"))
})


context("wordlist")


test_that("'abbreviations' has common acronyms", {
    expect_true(all(c("Mr.", "Mrs.", "Ms.") %in% abbreviations("english")))
    expect_true(all(c("ap. J.-C.", "av. J.-C.") %in% abbreviations("french")))
})


test_that("'abbreviations(NULL)' is NULL", {
    expect_equal(abbreviations(NULL), NULL)
    expect_equal(.Call(C_abbreviations, NULL), NULL)
})


test_that("'abbreviations(NA)' is NULL", {
    expect_equal(abbreviations(NA), NULL)
    expect_equal(.Call(C_abbreviations, NA_character_), NULL)
})


test_that("'abbreviations' errors for unknown kinds", {
    expect_error(abbreviations("xyz"),
                 '^unknown kind \\("xyz"\\)$')
})


test_that("'abbreviations' errors for non-string", {
    expect_error(abbreviations(1),
                 "^'kind' must be a character vector$")
})


test_that("'abbreviations' can union lists", {
    x1 <- abbreviations("english")
    x2 <- abbreviations("french")
    x <- abbreviations(c("english", "french"))
    expect_equal(x, sort(unique(c(x1, x2))))
})


test_that("'stopwords' has common function words", {
    expect_true(all(c("the", "and", "is") %in% stopwords_en))
})

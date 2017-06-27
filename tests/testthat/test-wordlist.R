
context("wordlist")


test_that("'abbreviations' has common acronyms", {
    expect_true(all(c("Mr.", "Mrs.", "Ms.") %in% abbreviations("english")))
    expect_true(all(c("ap. J.-C.", "av. J.-C.") %in% abbreviations("french")))
})


test_that("'abbreviations(NULL)' is NULL", {
    expect_equal(abbreviations(NULL), NULL)
})


test_that("'abbreviations' errors for unknown kinds", {
    expect_error(abbreviations("xyz"),
                 "^unknown abbreviations kind: \"xyz\"$")
})


test_that("'abbreviations' errors for unknown kinds", {
    expect_error(abbreviations("xyz"),
                 "^unknown abbreviations kind: \"xyz\"$")
})


test_that("'abbreviations' can union lists", {
    x1 <- abbreviations("english")
    x2 <- abbreviations("french")
    x <- abbreviations(c("english", "french"))
    expect_equal(x, sort(unique(c(x1, x2))))
})

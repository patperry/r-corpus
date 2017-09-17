
context("wordlist")


test_that("'abbreviations' has common acronyms", {
    expect_true(all(c("Mr.", "Mrs.", "Ms.") %in% abbreviations_en))
    expect_true(all(c("ap. J.-C.", "av. J.-C.") %in% abbreviations_fr))
})


test_that("'stopwords' has common function words", {
    expect_true(all(c("the", "and", "is") %in% stopwords_en))
})

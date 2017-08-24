context("gutenberg_corpus")

test_that("'gutenberg_corpus' can download Jules Verne in French", {
    if (!identical(Sys.getenv("WEB_RESOURCE_TESTS"), "true")) {
        skip("Not on Travis")
    }

    data <- gutenberg_corpus(800, verbose = FALSE)
    expect_equal(data$title, "Le Tour du Monde en 80 Jours")
    expect_equal(data$author, "Jules Verne")
    expect_equal(data$language, "French")
    expect_equal(nchar(as.character(data$text)), 421335)
})

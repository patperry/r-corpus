context("gutenberg_corpus")

test_that("'gutenberg_corpus' can download Jules Verne in French", {
    if (!identical(Sys.getenv("TEST_WEB_RESOURCES"), "true")) {
        skip("Not running web resource tests")
    }

    data <- gutenberg_corpus(800, verbose = FALSE)
    expect_equal(data$title, "Le Tour du Monde en 80 Jours")
    expect_equal(data$author, "Jules Verne")
    expect_equal(data$language, "French")
    expect_equal(nchar(as.character(data$text)), 421335)
})


test_that("'gutenberg_corpus' can handle NA", {
    data <- gutenberg_corpus(NA)
    expect_equal(data, corpus_frame(title = NA_character_,
                                    author = NA_character_,
                                    language = NA_character_,
                                    text = NA_character_))
})

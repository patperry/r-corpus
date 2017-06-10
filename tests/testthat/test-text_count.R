context("text_count")


test_that("text_count can works on sentences", {
    text <- c(a="He said, 'Are you going?' John Shook his head.",
              b="'Are you going?' John asked",
              c="This. Is. A. Long. Sentence!!!",
              d="Why all the shouting??")
    n0 <- text_count(text, "sentences")
    split <- text_split(text, "sentences")
    n <- c(with(split, tapply(index, parent, length)))
    names(n) <- names(text)
    expect_equal(n, n0)
})


test_that("text_count can works on tokens", {
    text <- c(a="He said, 'Are you going?' John Shook his head.",
              b="'Are you going?' John asked",
              c="This. Is. A. Long. Sentence!!!",
              d="Why all the shouting??")
    n0 <- text_count(text, "tokens")
    split <- text_split(text, "tokens")
    n <- c(with(split, tapply(index, parent, length)))
    names(n) <- names(text)
    expect_equal(n, n0)
})


test_that("text_count works on types", {
    expect_equal(text_count(LETTERS, "types", group = rep(1:2, 13)),
                 c("1" = 13, "2" = 13))

    expect_equal(text_count(paste(LETTERS, letters, LETTERS), "types"),
                 rep(1, 26))
})

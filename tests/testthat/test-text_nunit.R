context("text_nunit")


test_that("text_nsentence can works on sentences", {
    text <- c(a="He said, 'Are you going?' John Shook his head.",
              b="'Are you going?' John asked",
              c="This. Is. A. Long. Sentence!!!",
              d="Why all the shouting??")
    n0 <- text_nsentence(text)
    split <- text_split(text, "sentences")
    n <- c(with(split, tapply(index, parent, length)))
    names(n) <- names(text)
    expect_equal(n, n0)
})


test_that("text_nsentence handles NA and empty", {
    expect_equal(text_nsentence(c(NA, "")), c(NA, 0))
})


test_that("text_ntoken can works on tokens", {
    text <- c(a="He said, 'Are you going?' John Shook his head.",
              b="'Are you going?' John asked",
              c="This. Is. A. Long. Sentence!!!",
              d="Why all the shouting??")
    n0 <- text_ntoken(text)
    split <- text_split(text, "tokens")
    n <- c(with(split, tapply(index, parent, length)))
    names(n) <- names(text)
    expect_equal(n, n0)
})


test_that("text_ntoken handles NA and empty", {
    expect_equal(text_ntoken(c(NA, "")), c(NA, 0))
})


test_that("text_length includes NAs, text_ntoken doesn't", {
    x <- c("A man, a plan.", "A \"canal\"?", "Panama!")
    f <- text_filter(drop_punct = TRUE)
    toks <- text_tokens(x, f)
    ntok <- text_ntoken(x, f)
    len <- text_length(x, f)
    expect_equal(len, sapply(toks, length))
    expect_equal(ntok, len - sapply(toks, function(t) sum(is.na(t))))
})

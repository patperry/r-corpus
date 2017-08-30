context("text_filter")


test_that("'text_filter' has the right defaults", {
    f <- text_filter()
    expect_equal(f$map_case, TRUE)
    expect_equal(f$map_quote, TRUE)
    expect_equal(f$remove_ignorable, TRUE)
    expect_equal(f$stemmer, NULL)
    expect_equal(f$stem_dropped, FALSE)
    expect_equal(f$stem_except, NULL)
    expect_equal(f$combine, abbreviations("english"))
    expect_equal(f$drop_letter, FALSE)
    expect_equal(f$drop_number, FALSE)
    expect_equal(f$drop_punct, FALSE)
    expect_equal(f$drop_symbol, FALSE)
    expect_equal(f$drop, NULL)
    expect_equal(f$drop_except, NULL)
    expect_equal(f$sent_crlf, FALSE)
    expect_equal(f$sent_suppress, abbreviations("english"))
})


test_that("'text_filter' has the same defaults for all objects", {
    x <- c("hello", "world", "how", "are", "you?")
    y <- as_text(x)
    z <- data.frame(text = x)
    w <- data.frame(text = y)
    expect_equal(text_filter(x), text_filter())
    expect_equal(text_filter(y), text_filter())
    expect_equal(text_filter(z), text_filter())
    expect_equal(text_filter(w), text_filter())
})


test_that("'text_filter' can be assigned to text", {
    x <- as_text("hello")
    f0 <- text_filter(x)
    d <- data.frame(text = x)
    f <- text_filter(map_case = FALSE)
    text_filter(x) <- f
    expect_equal(text_filter(x), f)
    expect_equal(text_filter(d), f0)
})


test_that("'text_filter' can be assigned to data frame with \"text\" column", {
    x <- as_text("hello")
    f0 <- text_filter(x)
    d <- data.frame(text = x)
    f <- text_filter(map_case = FALSE)
    text_filter(d) <- f
    expect_equal(text_filter(x), f0)
    expect_equal(text_filter(d), f)
})


test_that("'text_filter' fails without data frame with \"text\" column", {
    x <- as_text("hello")
    d <- data.frame(not_text = x)
    f <- text_filter(map_case = FALSE)
    expect_error(text_filter(d), "no column named \"text\" in data frame")
    expect_error(text_filter(d) <- f, "no column named \"text\" in data frame")
})


test_that("'text_filter' cannot be assigned to character", {
    x <- "hello"
    d <- data.frame(text = x, stringsAsFactors = FALSE)
    f <- text_filter(map_case = FALSE)
    expect_error(text_filter(x) <- f,
                 "setting a text filter for objects of class \"character\" is not allowed",
                 fixed = TRUE)
    expect_error(text_filter(d) <- f,
                 "setting a text filter for objects of class \"character\" is not allowed",
                 fixed = TRUE)
})


test_that("setting an unrecognized property gives an error", {
    f <- text_filter()
    expect_error(f$foo <- "bar",
                 "unrecognized text filter property: \"foo\"",
                 fixed = TRUE)

    expect_error(text_filter(foo = "bar"),
                 "unrecognized text filter property: \"foo\"",
                 fixed = TRUE)
})


test_that("passing unnamed arguments is not allowed", {
    expect_error(text_filter(NULL, TRUE),
                 "unnamed arguments are not allowed")

    x <- as_text("hello")
    expect_error(text_filter(x, TRUE),
                 "unnamed arguments are not allowed")
})


test_that("giving invalid text to text_filter.corpus_text is not allowed", {
    expect_error(text_filter.corpus_text("hello"),
                 "argument is not a valid text object")
})


test_that("'as_text' propagates a non-NULL filter argument to character", {
    x <- "hello"
    f <- text_filter(map_case = FALSE)
    x1 <- as_text("hello", filter = f)
    x2 <- as_text("hello")
    x3 <- as_text("world", filter = f)
    expect_false(isTRUE(all.equal(x1, x2)))
    expect_false(isTRUE(all.equal(x1, x3)))
    expect_equal(text_filter(x1), f)
})


test_that("'as_text' propagates a non-NULL to text filter to text", {
    x <- as_text("hello")
    f0 <- text_filter(map_case = FALSE, map_quote = FALSE)
    text_filter(x) <- f0
    expect_equal(text_filter(x), f0)

    f1 <- text_filter(map_case = TRUE, map_quote = FALSE)
    y <- as_text(x, filter = f1)
    expect_equal(text_filter(y), f1)
})


test_that("'as_text' propagates a non-NULL to text filter to data frame", {
    d <- data.frame(text = "hello")
    f0 <- text_filter(d)

    f <- text_filter(map_case = FALSE)
    x <- as_text(d, filter = f)
    expect_equal(text_filter(d), f0)
    expect_equal(text_filter(x), f)
})


test_that("'as_text' propagates a non-NULL to filter to text data frame", {
    d <- data.frame(text = as_text("hello"))
    f0 <- text_filter(d)

    f <- text_filter(map_case = FALSE)
    x <- as_text(d, filter = f)
    expect_equal(text_filter(d), f0)
    expect_equal(text_filter(x), f)
})


test_that("'as_text' with NULL filter leaves it unchanged", {
    x <- as_text("hello")
    f0 <- text_filter(map_case = FALSE, map_quote = FALSE)
    text_filter(x) <- f0

    y <- as_text(x, filter = NULL)
    expect_equal(text_filter(y), f0)
})


test_that("'text_filter' clears the old filter", {
    x <- as_text("wicked")
    y <- as_text(x, filter = text_filter(stemmer = "english"))

    toks1 <- text_tokens(y)
    expect_equal(text_tokens(y), list("wick"))

    toks2 <- text_tokens(x)
    expect_equal(toks2, list("wicked"))
})

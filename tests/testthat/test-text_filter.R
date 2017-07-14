context("text_filter")


test_that("'text_filter' has the right defaults", {
    f <- text_filter()
    expect_equal(f$map_case, TRUE)
    expect_equal(f$map_compat, TRUE)
    expect_equal(f$map_quote, TRUE)
    expect_equal(f$remove_ignorable, TRUE)
    expect_equal(f$stemmer, NA_character_)
    expect_equal(f$stem_except, NULL)
    expect_equal(f$combine, abbreviations("english"))
    expect_equal(f$drop_letter, FALSE)
    expect_equal(f$drop_mark, FALSE)
    expect_equal(f$drop_number, FALSE)
    expect_equal(f$drop_punct, FALSE)
    expect_equal(f$drop_symbol, FALSE)
    expect_equal(f$drop_other, FALSE)
    expect_equal(f$drop, NULL)
    expect_equal(f$drop_except, NULL)
    expect_equal(f$crlf_break, FALSE)
    expect_equal(f$suppress, abbreviations("english"))
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


test_that("'text_filter' can be assigned to data frame", {
    x <- as_text("hello")
    f0 <- text_filter(x)
    d <- data.frame(text = x)
    f <- text_filter(map_case = FALSE)
    text_filter(d) <- f
    expect_equal(text_filter(x), f0)
    expect_equal(text_filter(d), f)
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
})

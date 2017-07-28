context("text_primitive")


test_that("subsetting should work", {
    x <- as_text(LETTERS)
    i <- c(7, 2, 3, 21, 15)
    y <- x[i]
    expect_equal(y, as_text(LETTERS[i]));
})


test_that("subsetting should retain filter", {
    f <- text_filter(map_case = FALSE)
    x <- as_text(LETTERS, filter = f)
    i <- c(7, 2, 3, 21, 15)
    y <- x[i]
    expect_equal(text_filter(y), f)
})


test_that("anyNA should work", {
    x <- as_text(c("a", NA, "", "b"))
    y <- as_text(c())
    z <- as_text(letters)

    expect_true(anyNA(x))
    expect_false(anyNA(y))
    expect_false(anyNA(z))
})


test_that("converting to character should work", {
    x <- c("hello", NA, "world", "")
    y <- as_text(x)
    expect_equal(as.character(y), x)
})


test_that("conversions should work", {
    expect_equal(as.complex(as_text("1+2i")), 1+2i)
    expect_equal(as.double(as_text("3.14")), 3.14)
    expect_equal(as.integer(as_text("3.14")), 3)
    expect_equal(as.logical(as_text(c("TRUE", "FALSE", "NA"))),
                 c(TRUE, FALSE, NA))
    expect_equal(as.numeric(as_text("3.14")), 3.14)
    expect_equal(as.raw(as_text("1")), as.raw("1"))

    expect_warning(x <- as.numeric(as_text("foo")),
                   "NAs introduced by coercion")
    expect_equal(x, NA_real_)
})


test_that("is.na should work", {
    x <- as_text(c("a", NA, "", "b"))
    expect_equal(is.na(x), c(FALSE, TRUE, FALSE, FALSE))
    expect_equal(is.na(as_text(c())), logical())
})


test_that("rep should work", {
    x <- as_text(c("a", "b", "c"))
    y <- rep(x, 7)
    expect_equal(y, as_text(rep(c("a", "b", "c"), 7)))
})



test_that("invalid operations should error", {
    x <- as_text("hello")
    expect_error(x$names, "$ operator is invalid for text objects",
                 fixed = TRUE)
    expect_error(x$names <- "foo", "$<- operator is invalid for text objects",
                 fixed = TRUE)
    expect_error(as.environment(x),
                 "'as.environment' is invalid for text objects")
})


test_that("text methods should error for non-text", {
    expect_error(`[.corpus_text`("hello", 1), "invalid text object")
})

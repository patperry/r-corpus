context("text_primitive")


test_that("anyNA should work", {
    x <- as_corpus_text(c("a", NA, "", "b"))
    y <- as_corpus_text(c())
    z <- as_corpus_text(letters)

    expect_true(anyNA(x))
    expect_false(anyNA(y))
    expect_false(anyNA(z))
})


test_that("converting to character should work", {
    x <- c("hello", NA, "world", "")
    y <- as_corpus_text(x)
    expect_equal(as.character(y), x)
})


test_that("conversions should work", {
    expect_equal(as.complex(as_corpus_text("1+2i")), 1+2i)
    expect_equal(as.double(as_corpus_text("3.14")), 3.14)
    expect_equal(as.integer(as_corpus_text("3.14")), 3)
    expect_equal(as.logical(as_corpus_text(c("TRUE", "FALSE", "NA"))),
                 c(TRUE, FALSE, NA))
    expect_equal(as.numeric(as_corpus_text("3.14")), 3.14)
    expect_equal(as.raw(as_corpus_text("1")), as.raw("1"))

    expect_warning(x <- as.numeric(as_corpus_text("foo")),
                   "NAs introduced by coercion")
    expect_equal(x, NA_real_)
})


test_that("is.na should work", {
    x <- as_corpus_text(c("a", NA, "", "b"))
    expect_equal(is.na(x), c(FALSE, TRUE, FALSE, FALSE))
    expect_equal(is.na(as_corpus_text(c())), logical())
})


test_that("rep should work", {
    x <- as_corpus_text(c("a", "b", "c"))
    y <- rep(x, 7)
    expect_equal(y, as_corpus_text(rep(c("a", "b", "c"), 7)))
})


test_that("rep should work with names", {
    x <- as_corpus_text(c(x="a", y="b"))
    y <- rep(x, 2)
    expect_equal(y, as_corpus_text(c(x="a", y="b", x.1="a", y.1="b")))
})


test_that("invalid operations should error", {
    x <- as_corpus_text("hello")
    expect_error(x$names, "$ operator is invalid for text objects",
                 fixed = TRUE)
    expect_error(x$names <- "foo", "$<- operator is invalid for text objects",
                 fixed = TRUE)
    expect_error(as.environment(x),
                 "'as.environment' is invalid for text objects")
})

test_that("setting length on invalid text should fail", {
    x <- letters
    expect_error(`length<-.corpus_text`(x, 5), "invalid text object")
})

test_that("setting invalid length should fail", {
    x <- as_corpus_text(letters)
    expect_error(length(x) <- NULL, "'length' cannot be NULL")
    expect_error(length(x) <- "1", "'length' must be numeric")
    expect_error(length(x) <- c(1, 1), "'length' must have length 1")
    expect_error(length(x) <- NA, "'length' cannot be NA")
    expect_error(length(x) <- NaN, "'length' cannot be NaN")
    expect_error(length(x) <- -1, "'length' cannot be negative")
    expect_error(length(x) <- 2^53 + 2, "'length' cannot be above 2\\^53")
})


test_that("setting short length should work", {
    x <- as_corpus_text(letters)
    length(x) <- 10
    expect_equal(x, as_corpus_text(letters[1:10]))
})


test_that("setting same length should work", {
    x <- as_corpus_text(letters)
    length(x) <- 26
    expect_equal(x, as_corpus_text(letters))
})


test_that("setting long length should work", {
    x <- as_corpus_text(letters)
    length(x) <- 30
    expect_equal(x, as_corpus_text(c(letters, rep(NA, 4))))
})

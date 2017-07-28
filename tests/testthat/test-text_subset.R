context("text_subset")

test_that("invalid subscript operations should error", {
    x <- as_text(LETTERS)

    expect_error(x[[integer()]], "attempt to select less than one element")
    expect_error(x[[integer()]] <- character(),
                 "attempt to select less than one element")

    expect_error(x[[1:2]], "attempt to select more than one element")
    expect_error(x[[1:2]] <- c("a", "b"),
                 "attempt to select more than one element")

    expect_error(x[[1]] <- character(), "replacement has length zero")
    expect_error(x[[1]] <- c("a", "b"),
                 "more elements supplied than there are to replace")

    expect_error(`[[.corpus_text`("hello", 1), "invalid text object")
    expect_error(`[[<-.corpus_text`("hello", 1, NA), "invalid text object")
})


test_that("subset should retain attributes", {
    f <- text_filter(map_case = FALSE)
    x <- as_text(LETTERS, filter = f)
    names(x) <- letters
    attr(x, "foo") <- "bar"

    expect_equal(names(x[1:3]), letters[1:3])
    expect_equal(text_filter(x[1:3]), f)
    expect_equal(attr(x, "foo"), "bar")
})


test_that("subset assign should retain attributes", {
    f <- text_filter(map_case = FALSE)
    x <- as_text(LETTERS, filter = f)
    names(x) <- letters
    attr(x, "foo") <- "bar"

    x[1:3] <- NA

    expect_equal(names(x), letters)
    expect_equal(text_filter(x), f)
    expect_equal(attr(x, "foo"), "bar")
})


test_that("subsetting empty should work", {
    x <- as_text(LETTERS)
    expect_equal(x[], x)

    x[] <- letters
    expect_equal(x, as_text(letters))
})


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


test_that("text methods should error for non-text", {
    expect_error(`[.corpus_text`("hello", 1), "invalid text object")
})


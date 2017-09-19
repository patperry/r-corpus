context("text_index")

test_that("invalid subscript operations should error", {
    x <- as_corpus_text(LETTERS)

    expect_error(x[[integer()]], "attempt to select less than one element")
    expect_error(x[[integer()]] <- character(),
                 "attempt to select less than one element")

    expect_error(x[[1:2]], "attempt to select more than one element")
    expect_error(x[[1:2]] <- c("a", "b"),
                 "attempt to select more than one element")

    expect_error(x[[1]] <- character(), "replacement has length zero")
    expect_error(x[[1]] <- c("a", "b"),
                 "more elements supplied than there are to replace")

    expect_error(x[[list(1)]] <- "A",
                 "invalid subscript type 'list'")

    expect_error(`[[.corpus_text`("hello", 1), "invalid text object")
    expect_error(`[[<-.corpus_text`("hello", 1, NA), "invalid text object")
})


test_that("subset should retain attributes", {
    f <- text_filter(map_case = FALSE)
    x <- as_corpus_text(LETTERS, filter = f)
    names(x) <- letters
    attr(x, "foo") <- "bar"

    expect_equal(names(x[1:3]), letters[1:3])
    expect_equal(text_filter(x[1:3]), f)
    expect_equal(attr(x, "foo"), "bar")
})


test_that("subset assign should retain attributes", {
    f <- text_filter(map_case = FALSE)
    x <- as_corpus_text(LETTERS, filter = f)
    names(x) <- letters
    attr(x, "foo") <- "bar"

    x[1:3] <- NA

    expect_equal(names(x), letters)
    expect_equal(text_filter(x), f)
    expect_equal(attr(x, "foo"), "bar")
})


test_that("subsetting empty should work", {
    x <- as_corpus_text(LETTERS)
    expect_equal(x[], x)

    x[] <- letters
    expect_equal(x, as_corpus_text(letters))
})


test_that("subsetting should work", {
    x <- as_corpus_text(LETTERS)
    i <- c(7, 2, 3, 21, 15)
    y <- x[i]
    expect_equal(y, as_corpus_text(LETTERS[i]))
})


test_that("subsetting should retain filter", {
    f <- text_filter(map_case = FALSE)
    x <- as_corpus_text(LETTERS, filter = f)
    i <- c(7, 2, 3, 21, 15)
    y <- x[i]
    expect_equal(text_filter(y), f)
})


test_that("subset assign can have duplicate integer indices", {
    x <- as_corpus_text(LETTERS)
    x[c(1, 1, 1)] <- "XXX"
    expect_equal(x, as_corpus_text(c("XXX", LETTERS[-1])))
})


test_that("subset assign can have new indices", {
    x <- as_corpus_text(LETTERS[1:5])
    x[10] <- "foo"
    expect_equal(x, as_corpus_text(c(LETTERS[1:5], rep(NA, 4), "foo")))
})


test_that("subset assign can have new names", {
    x <- as_corpus_text(LETTERS[1:3])
    x["foo"] <- "bar"
    expect_equal(x, as_corpus_text(c("1" = "A", "2" = "B", "3" = "C", foo = "bar")))
})


test_that("subset assign can have duplicate names", {
    x <- as_corpus_text(LETTERS[1:3])
    x[c("foo", "foo")] <- c("bar", "baz")
    expect_equal(x, as_corpus_text(c("1" = "A", "2" = "B", "3" = "C", foo = "baz")))
})


test_that("subsetting should allow extending the object", {
    x <- as_corpus_text(letters)
    y <- as_corpus_text(LETTERS)
    x[27:52] <- y
    expect_equal(x, as_corpus_text(c(letters, LETTERS)))
})


test_that("subsetting with char should expand names before setting", {
    x <- as_corpus_text(letters[1:5])
    x["3"] <- "ZZZ"
    expect_equal(x, as_corpus_text(c("1" = "a", "2" = "b", "3" = "ZZZ",
                              "4" = "d", "5" = "e")))
})


test_that("text methods should error for non-text", {
    expect_error(`[<-.corpus_text`("hello", 1, "a"), "invalid text object")
    expect_error(`[.corpus_text`("hello", 1), "invalid text object")
})


test_that("rbind should take filter from first value", {
    f <- text_filter(map_case = FALSE)
    x <- data.frame(text = as_corpus_text(letters, filter = f), row.names = NULL)
    y <- data.frame(text = as_corpus_text(LETTERS), row.names = NULL)
    z <- rbind(x, y)
    expect_equal(z, data.frame(text = as_corpus_text(c(letters, LETTERS), filter = f),
                               row.names = NULL))

    z2 <- rbind(y, x)
    expect_equal(z2, data.frame(text = as_corpus_text(c(LETTERS, letters)),
                                row.names = NULL))
})


test_that("subset can drop unused sources", {
    x <- c(as_corpus_text("aaa"), as_corpus_text("b"), as_corpus_text("cc"))

    expect_equal(x[NULL], as_corpus_text(character()))
    expect_equal(x[1], as_corpus_text("aaa"))
    expect_equal(x[2], as_corpus_text("b"))
    expect_equal(x[3], as_corpus_text("cc"))
    expect_equal(x[c(1,2)], as_corpus_text(c("aaa", "b")))
    expect_equal(x[c(2,1)], as_corpus_text(c("b", "aaa")))
    expect_equal(x[c(1,3)], as_corpus_text(c("aaa", "cc")))
    expect_equal(x[c(3,1)], as_corpus_text(c("cc", "aaa")))
    expect_equal(x[c(2,3)], as_corpus_text(c("b", "cc")))
    expect_equal(x[c(3,2)], as_corpus_text(c("cc", "b")))
})

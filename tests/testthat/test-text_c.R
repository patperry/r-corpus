context("text_c")


test_that("c should not drop text_filter", {
    x <- as_corpus_text("hello")
    text_filter(x) <- text_filter(map_case = FALSE)
    y <- as_corpus_text("world")
    z <- c(x, y)
    expect_equal(text_filter(z), text_filter(x))
})


test_that("c should work with named or unnamed args", {
    x <- as_corpus_text("hello")
    y <- as_corpus_text("world")
    expect_equal(c(x, y), as_corpus_text(c("hello", "world")))
    expect_equal(c(a = x, b = y), as_corpus_text(c(a = "hello", b = "world")))
})


test_that("c should work with complex args", {
    x <- c(a=as_corpus_text(c("hello", NA, "world")), "!", c=7)
    expect_equal(x, as_corpus_text(c(a1 = "hello", a2 = NA, a3 = "world",
                              "4" = "!", c = "7")))
})


test_that("c should work with a single unnamed argument", {
    x0 <- as_corpus_text(c(a = "hello", b = "goodbye", "!"))
    x <- x0
    text_filter(x) <- text_filter(map_case = FALSE)
    y <- c(x)
    expect_equal(y, x)
})


test_that("c should work with a single named argument", {
    x <- as_corpus_text(c(a = "hello", b = "goodbye", "!"))
    y <- c(a = x)
    expect_equal(y, as_corpus_text(c(a.a = "hello", a.b = "goodbye", a3 = "!")))
})


test_that("c should work with no names", {
    x <- as_corpus_text(c("hello", "goodbye", "!"))
    z <- c(x, x)
    expect_equal(names(z), NULL)
    expect_equal(as.character(z), c(as.character(x), as.character(x)))
})


test_that("c should work with no arguments", {
    z <- c.corpus_text()
    expect_equal(z, as_corpus_text(c()))
})


test_that("c should support use.names = FALSE", {
    z <- c(a=as_corpus_text("x"), y=c(z="z", "w"), use.names = FALSE)
    expect_equal(z, as_corpus_text(c("x", "z", "w")))
})


test_that("c should support lists with recursive = TRUE", {
    z <- c.corpus_text(list(x = as_corpus_text("a"), y = as_corpus_text("b")), z = "c",
                       recursive = TRUE)
    expect_equal(z, as_corpus_text(c(x = "a", y = "b", z = "c")))
})


test_that("c should support pairlists with recursive = TRUE", {
    z <- c.corpus_text(pairlist(x = as_corpus_text("a"), y = as_corpus_text("b")), z = "c",
                       recursive = TRUE)
    expect_equal(z, as_corpus_text(c(x = "a", y = "b", z = "c")))
})


test_that("c can handle NA after named", {
    z <- c(as_corpus_text(c(x = "a")), NA)
    expect_equal(z, as_corpus_text(c(x = "a", "2" = NA)))
})


test_that("c should take filter from first value", {
    f <- text_filter(map_case = FALSE)
    x <- as_corpus_text(letters, filter = f)
    y <- as_corpus_text(LETTERS)
    z <- c(x, y)
    expect_equal(z, as_corpus_text(c(letters, LETTERS), filter = f))

    z2 <- c(y, x)
    expect_equal(z2, as_corpus_text(c(LETTERS, letters)))
})


test_that("c should work with duplicate names", {
    x <- as_corpus_text(c(a = "hello", b = "goodbye", "!"))
    z <- c(x, x)
    expect_equal(names(z), c(names(x), paste0(names(x), ".1")))
    expect_equal(as.character(z), c(as.character(x), as.character(x)))
})

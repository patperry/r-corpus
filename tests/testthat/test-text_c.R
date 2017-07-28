context("text_c")


test_that("c should drop text_filter", {
    x <- as_text("hello")
    text_filter(x) <- text_filter(map_case = FALSE)
    y <- as_text("world")
    z <- c(x, y)
    expect_equal(text_filter(z), text_filter())
})


test_that("c should work with named or unnamed args", {
    x <- as_text("hello")
    y <- as_text("world")
    expect_equal(c(x, y), as_text(c("hello", "world")))
    expect_equal(c(a = x, b = y), as_text(c(a = "hello", b = "world")))
})


test_that("c should work with complex args", {
    x <- c(a=as_text(c("hello", NA, "world")), "!", c=7)
    expect_equal(x, as_text(c(a1 = "hello", a2 = NA, a3 = "world", "!",
                              c = "7")))
})


test_that("c should work with a single unnamed argument", {
    x0 <- as_text(c(a = "hello", b = "goodbye", "!"))
    x <- x0
    text_filter(x) <- text_filter(map_case = FALSE)
    y <- c(x)
    expect_equal(y, x0)
})


test_that("c should work with a single named argument", {
    x <- as_text(c(a = "hello", b = "goodbye", "!"))
    y <- c(a = x)
    expect_equal(y, as_text(c(a.a = "hello", a.b = "goodbye", a3 = "!")))
})


test_that("c should work with duplicate names", {
    x <- as_text(c(a = "hello", b = "goodbye", "!"))
    z <- c(x, x)
    expect_equal(names(z), c(names(x), names(x)))
    expect_equal(as.character(z), c(as.character(x), as.character(x)))
})


test_that("c should work with no names", {
    x <- as_text(c("hello", "goodbye", "!"))
    z <- c(x, x)
    expect_equal(names(z), NULL)
    expect_equal(as.character(z), c(as.character(x), as.character(x)))
})


test_that("c should work with no arguments", {
    z <- c.corpus_text()
    expect_equal(z, as_text(c()))
})


test_that("c should support use.names = FALSE", {
    z <- c(a=as_text("x"), y=c(z="z", "w"), use.names = FALSE)
    expect_equal(z, as_text(c("x", "z", "w")))
})

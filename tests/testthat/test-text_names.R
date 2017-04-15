context("text_names")


test_that("`names` should be NULL for new text", {
    x <- text("ABC")
    expect_equal(names(x), NULL)
})


test_that("`names<-` should work on text", {
    x <- text(LETTERS)
    names(x) <- rev(LETTERS)
    expect_equal(names(x), rev(LETTERS))
})


test_that("`as.text` should drop names", {
    x <- as.text(c(a="1", b="2"))
    expect_equal(names(x), NULL)

    y <- as.text(text(a="1", b="2"))
    expect_equal(names(y), NULL)
})


test_that("`all.equal` should test names", {
    x <- text(1:3)
    y <- x
    names(y) <- c("a", "b", "c")
    expect_equal(all.equal(x, y), "names for current but not for target")
    expect_equal(all.equal(y, x), "names for target but not for current")
})


test_that("`as.text` should drop names", {
    x <- text(foo="hello")
    y <- as.text(x)

    expect_equal(x, text(foo="hello"))
    expect_equal(y, text("hello"))
})


test_that("`as.text` should drop attributes", {
    x <- text("hello")
    attr(x, "foo") <- "bar"
    y <- as.text(x)

    expect_equal(y, text("hello"))
})


test_that("`names<-` should not modify copies", {
    x <- text(1:3)
    y <- x
    names(y) <- c("a", "b", "c")
    expect_equal(names(x), NULL)
    expect_equal(names(y), c("a", "b", "c"))
})


test_that("`names<-` should preserve attributes", {
    x <- text(1:3)
    attr(x, "foo") <- "bar"
    names(x) <- c("a", "b", "c")
    expect_equal(names(x), c("a", "b", "c"))
    expect_equal(attr(x, "foo"), "bar")
})

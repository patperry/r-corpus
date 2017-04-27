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


test_that("`as_text` should not drop names", {
    x <- as_text(c(a="1", b="2"))
    expect_equal(names(x), c("a", "b"))

    y <- as_text(text(a="1", b="2"))
    expect_equal(names(y), c("a", "b"))
})


test_that("`all.equal` should test names", {
    x <- text(1:3)
    y <- x
    names(y) <- c("a", "b", "c")
    expect_equal(all.equal(x, y), "names for current but not for target")
    expect_equal(all.equal(y, x), "names for target but not for current")
})


test_that("`as_text` should not drop names", {
    x <- text(foo="hello")
    y <- as_text(x)

    expect_equal(y, text(foo="hello"))
})


test_that("`as_text` should drop attributes", {
    x <- text("hello")
    attr(x, "foo") <- "bar"
    y <- as_text(x)

    expect_equal(y, text("hello"))
})


test_that("`as_text` should drop attributes for JSON objects", {
    file <- tempfile()
    writeLines('{"text": "hello"}', file)
    x <- read_json(file)$text

    attr(x, "foo") <- "bar"
    y <- as_text(x)

    expect_equal(y, text("hello"))

    rm("x", "y"); gc(); file.remove(file)
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


test_that("`names<-` should allow NA", {
    x <- text(1:3)
    names(x) <- c("a", NA, "b")
    expect_equal(names(x), c("a", NA, "b"))
})


test_that("`names<-` should allow duplicates", {
    x <- text(1:3)
    names(x) <- c("a", "b", "a")
    expect_equal(names(x), c("a", "b", "a"))
})

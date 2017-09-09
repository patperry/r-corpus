context("text_names")


test_that("`names` should be NULL for new text", {
    x <- as_corpus_text(c("A", "B", "C"))
    expect_equal(names(x), NULL)

    expect_equal(names(as_corpus_text(character())), NULL)
})


test_that("`names<-` should work on text", {
    x <- as_corpus_text(LETTERS)
    names(x) <- rev(LETTERS)
    expect_equal(names(x), rev(LETTERS))
})


test_that("setting `names<-` to NULL should restore defaults", {
    x <- as_corpus_text(c(a="x", b="y"))
    names(x) <- NULL
    expect_equal(names(x), NULL)
})


test_that("`as_corpus_text` should not drop names", {
    x <- as_corpus_text(c(a="1", b="2"))
    expect_equal(names(x), c("a", "b"))
})


test_that("`all.equal` should test names", {
    x <- as_corpus_text(1:3)
    y <- x
    names(y) <- c("a", "b", "c")
    expect_equal(all.equal(x, y), "names for current but not for target")
    expect_equal(all.equal(y, x), "names for target but not for current")
})


test_that("`as_corpus_text` should not drop names", {
    x <- as_corpus_text(c(foo="hello"))
    y <- as_corpus_text(x)

    expect_equal(y, as_corpus_text(c(foo="hello")))
})


test_that("`as_corpus_text` should drop attributes", {
    x <- as_corpus_text("hello")
    attr(x, "foo") <- "bar"
    y <- as_corpus_text(x)

    expect_equal(y, as_corpus_text("hello"))
})


test_that("`as_corpus_text` should drop attributes for JSON objects", {
    file <- tempfile()
    writeLines('{"text": "hello"}', file)
    x <- read_ndjson(file)$text

    attr(x, "foo") <- "bar"
    y <- as_corpus_text(x)

    expect_equal(y, as_corpus_text("hello"))
})


test_that("`names<-` should not modify copies", {
    x <- as_corpus_text(1:3)
    y <- x
    names(y) <- c("a", "b", "c")
    expect_equal(names(x), NULL)
    expect_equal(names(y), c("a", "b", "c"))
})


test_that("`names<-` should preserve attributes", {
    x <- as_corpus_text(1:3)
    attr(x, "foo") <- "bar"
    names(x) <- c("a", "b", "c")
    expect_equal(names(x), c("a", "b", "c"))
    expect_equal(attr(x, "foo"), "bar")
})


test_that("`names<-` should not allow NA", {
    x <- as_corpus_text(1:3)
    expect_error(names(x) <- c("a", NA, "b"),
                 "missing values in 'names' are not allowed")
})


test_that("`names<-` should not allow duplicates", {
    x <- as_corpus_text(1:3)
    expect_error(names(x) <- c("a", "b", "a"),
                 "duplicate 'names' are not allowed")
})


test_that("names should error for non-text", {
    expect_error(names.corpus_text("hello"), "invalid text object")
})

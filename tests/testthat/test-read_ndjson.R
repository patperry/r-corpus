context("read_ndjson")


test_that("reading a non-existent file should fail", {
    corpus:::logging_off()
    expect_error(read_ndjson("foobar", mmap=TRUE), "cannot open file 'foobar'")
    corpus:::logging_on()
})


test_that("reading a factor with escapes should work", {
    file <- tempfile()
    writeLines(c('"an\\nescape"', '"an\\u000aescape"'), file)
    x <- read_ndjson(file)
    expect_equal(levels(x), "an\nescape")
    expect_equal(as.integer(x), c(1, 1))
})


test_that("reading a factor with nulls should work", {
    file <- tempfile()
    writeLines(c('"A"', 'null', '"B"'), file)
    x <- read_ndjson(file)
    expect_equal(levels(x), c("A", "B"))
    expect_equal(as.integer(x), c(1, NA, 2))
})

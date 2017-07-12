context("read_ndjson")


test_that("reading a non-existent file should fail", {
    corpus:::logging_off()
    expect_error(read_ndjson("foobar", mmap=TRUE), "cannot open file 'foobar'")
    corpus:::logging_on()
})


test_that("passing a connection when mmap is TRUE should fail", {
    tmp <- tempfile()
    writeLines(character(), tmp)
    file <- file(tmp)
    on.exit(close(file))
    expect_error(read_ndjson(file, mmap = TRUE),
                 "'file' must be a character string when 'mmap' is TRUE")
})


test_that("passing a file name should succeed", {
    file <- tempfile()
    writeLines('"foo"', file)
    expect_equal(read_ndjson(file), "foo")
})


test_that("passing a closed connection should succeed", {
    tmp <- tempfile()
    file <- file(tmp)
    writeLines('"foo"', file)
    expect_equal(read_ndjson(file), "foo")
})


test_that("passing an empty file should succeed", {
    file <- tempfile()
    writeLines(character(), file)
    expect_equal(read_ndjson(file), NULL)
})


test_that("passing a nonscalar should fail", {
    expect_error(read_ndjson(17),
                 "'file' must be a character string or connection")
})

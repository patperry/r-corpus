context("read_ndjson")


test_that("reading a non-existent file should fail", {
    corpus:::logging_off()
    expect_error(read_ndjson("foobar", mmap=TRUE), "cannot open file 'foobar'")
    corpus:::logging_on()
})

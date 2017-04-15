context("read_json")


test_that("reading a non-existent file should fail", {
    corpus:::logging_off()
    expect_error(read_json("foobar"), "cannot open file 'foobar'")
    corpus:::logging_on()
})

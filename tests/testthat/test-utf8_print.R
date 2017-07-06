context("utf8_print")

test_that("'utf8_print' works with unnamed character vectors", {
    x <- as.character(1:100)

    expect_equal(capture_output(utf8_print(x)),
                 capture_output(print(x)))

    expect_equal(capture_output(utf8_print(x[1:96])),
                 capture_output(print(x[1:96])))

    expect_equal(capture_output(utf8_print(x[1:7])),
                 capture_output(print(x[1:7])))
})


test_that("'utf8_print' works with named character vectors", {
    x <- as.character(10 + 1:26)
    names(x) <- letters

    # left align names
    xr <- x
    names(xr) <- format(names(x), aligh="left", width = 4)
    actual <- strsplit(capture_output(utf8_print(x)), "\n")[[1]]
    expected <- strsplit(capture_output(print(xr)), "\n")[[1]]
    expect_equal(paste(actual, ""), expected)

    actual <- strsplit(capture_output(utf8_print(x[1:16])), "\n")[[1]]
    expected <- strsplit(capture_output(print(xr[1:16])), "\n")[[1]]
    expect_equal(paste(actual, ""), expected)

    actual <- strsplit(capture_output(utf8_print(x[1:4])), "\n")[[1]]
    expected <- strsplit(capture_output(print(xr[1:4])), "\n")[[1]]
    expect_equal(paste(actual, ""), expected)
})

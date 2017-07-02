
context("frame")

test_that("'print.corpus_frame' produces the same results on ASCII", {
    options(encoding = "UTF-8")
    dd <- data.frame(x = 1:10, f = gl(2,5), ch = I(letters[1:10]))

    expect_equal(capture_output(print.corpus_frame(dd)),
                 capture_output(print(dd)))
    expect_equal(
        capture_output(print.corpus_frame(dd, quote = TRUE, row.names = FALSE)),
        capture_output(print(dd, quote = TRUE, row.names = FALSE)))
})

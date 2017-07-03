
context("frame")

test_that("'print.corpus_frame' produces the same results on ASCII", {
    options(encoding = "UTF-8")
    d <- data.frame(x = 1:10, f = gl(2,5), ch = I(letters[1:10]))

    expect_equal(capture_output(print.corpus_frame(d)),
                 capture_output(print(d)))
    expect_equal(
        capture_output(print.corpus_frame(d, quote = TRUE, row.names = FALSE)),
        capture_output(print(d, quote = TRUE, row.names = FALSE)))
})


test_that("'print.corpus_frame' wraps correctly", {
    w <- getOption("width")
    half <- floor(w / 2)
    d <- data.frame(x = c("X", paste(rep("x", 2 * w), collapse="")),
                    y = c("Y", paste(rep("y", half + 1), collapse="")),
                    z = c("Z", paste(rep("z", half + 1), collapse="")),
                    a = 1:2, b = 3:4, c = 5:6)

    expect_equal(capture_output(print.corpus_frame(d)),
                 capture_output(print(d)))
})


test_that("'print.corpus_frame' handles NA in column names", {
    d <- data.frame(x = 1:10, f = gl(2,5), ch = I(letters[1:10]))
    colnames(d) <- c("x", NA, "ch")

    expect_equal(capture_output(print.corpus_frame(d)),
                 capture_output(print(d)))
})


test_that("'print.corpus_frame' handles NA elements", {
    d <- data.frame(x = NA, ch = I(NA_character_), f = as.factor(NA_character_))

    expect_equal(capture_output(print.corpus_frame(d)),
                 capture_output(print(d)))

    expect_equal(capture_output(print.corpus_frame(d, quote = TRUE)),
                 capture_output(print(d, quote = TRUE)))

    expect_equal(capture_output(print.corpus_frame(d, na.print = "foo")),
                 capture_output(print(d, na.print = "foo")))

    expect_equal(capture_output(print.corpus_frame(d, na.print = "foo", quote = TRUE)),
                 capture_output(print(d, na.print = "foo", quote = TRUE)))
})

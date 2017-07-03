
context("frame")

test_that("'print.corpus_frame' produces the same results on ASCII", {
    d <- data.frame(x = 1:10, f = gl(2,5), ch = I(letters[1:10]))

    expect_equal(capture_output(print.corpus_frame(d)),
                 capture_output(print(d)))
    expect_equal(
        capture_output(print.corpus_frame(d, quote = TRUE, row.names = FALSE)),
        capture_output(print(d, quote = TRUE, row.names = FALSE)))
})


test_that("'print.corpus_frame' handles row names", {
    d <- data.frame(x = 1:10, f = gl(2,5), ch = I(letters[1:10]))
    row.names(d) <- LETTERS[1:10]

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


test_that("'print.corpus_frame' handles empty data frames", {
    # no row or column names
    d1 <- data.frame()
    expect_equal(capture_output(print.corpus_frame(d1)),
                 "data frame with 0 columns and 0 rows")

    # no row names
    d2 <- data.frame(a = integer(), b = integer(), "\n" = logical(),
                     check.names = FALSE)
    expect_equal(capture_output(print.corpus_frame(d2)), " a b \\n\n(0 rows)")

    # columns but no column names
    d3 <- structure(list(integer(), integer()),
                    class = "data.frame", row.names = c(NA, 0))
    expect_equal(capture_output(print.corpus_frame(d3)),
                 "data frame with 2 columns and 0 rows")
})

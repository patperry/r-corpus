context("text_base")

test_that("invalid ops should fail", {
    x <- as_corpus_text("hello")
    expect_error(Re(x), "'Re' is not defined for text objects")
    expect_error(abs(x), "'abs' is not defined for text objects")
    expect_error(!x, "unary ! is not defined for text objects")
    expect_error(x + x, "+ is not defined for text objects")
    expect_error(as.matrix(x), "'as.matrix' is not defined for text objects")
    expect_error(cbind(x), "'cbind' is not defined for text objects")
    expect_error(rbind(x), "'rbind' is not defined for text objects")
    expect_error(solve(x), "'solve' is not defined for text objects")
    expect_error(t(x), "'t' is not defined for text objects")
})


test_that("character comparisons should work", {
    x <- letters
    y <- LETTERS
    tx <- as_corpus_text(x)
    ty <- as_corpus_text(y)
    expect_equal(tx < ty, x < y)
})


test_that("character comparisons should preserve names", {
    x <- c(a = "1", b = "2")
    y <- c(c = "3", d = "4")
    expect_equal(as_corpus_text(x) < as_corpus_text(y), x < y)
})

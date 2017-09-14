context("term_counts")

test_that("'term_counts' gives equivalent results to 'term_matrix'", {
    text <- c(a="A rose is a rose is a rose.",
              b="A Rose is red, a violet is blue!",
              c="A rose by any other name would smell as sweet.")
    x <- term_matrix(text)
    tf <- term_counts(text)
    xtf <- Matrix::sparseMatrix(i = as.integer(tf$text),
                                j = as.integer(tf$term),
                                x = tf$count,
                                dimnames = list(levels(tf$text),
                                                colnames(x)))
    expect_equal(x, xtf)
})


test_that("'term_counts' gives equivalent results to 'term_matrix' no names", {
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    x <- term_matrix(text)
    rownames(x) <- as.character(seq_along(text))
    tf <- term_counts(text)
    xtf <- Matrix::sparseMatrix(i = as.integer(tf$text),
                                j = as.integer(tf$term),
                                x = tf$count,
                                dimnames = list(levels(tf$text),
                                                colnames(x)))
    expect_equal(x, xtf)
})


test_that("'term_counts' with group gives equivalent results to 'term_matrix'", {
    text <- c(a="A rose is a rose is a rose.",
              b="A Rose is red, a violet is blue!",
              c="A rose by any other name would smell as sweet.")
    g <- factor(c("X", "Y", "X"))
    x <- term_matrix(text, group = g)
    tf <- term_counts(text, group = g)
    xtf <- Matrix::sparseMatrix(i = as.integer(tf$group),
                                j = as.integer(tf$term),
                                x = tf$count,
                                dimnames = list(levels(tf$group),
                                                colnames(x)))
    expect_equal(x, xtf)
})

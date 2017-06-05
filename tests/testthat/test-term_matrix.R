context("term_matrix")

test_that("'term_matrix' works", {
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    x <- term_matrix(text)

    # compute the term matrix by hand
    toks <- tokens(text)
    i <- rep(seq_along(toks), sapply(toks, length))
    toks <- factor(c(toks, recursive = TRUE))
    j <- as.integer(toks)
    x0 <- Matrix::sparseMatrix(i[!is.na(j)], j[!is.na(j)], x = 1,
                               dimnames=list(NULL, levels(toks)))

    # dimensions should agree
    expect_equal(dim(x), dim(x0))

    # column names should agree (but not necessarily be in same order)
    expect_true(all(colnames(x) %in% colnames(x0)))
    expect_true(all(colnames(x0) %in% colnames(x)))
    x0 <- x0[, colnames(x), drop = FALSE]

    # after re-ordering columns, the matrices should be equal
    expect_equal(x, x0)
})


test_that("'term_matrix' should handle empty texts", {
    x <- term_matrix(c(NA, "hello", "", NA, ""))
    x0 <- Matrix::sparseMatrix(i = 2, j = 1, x = 1, dims=c(5,1),
                               dimnames=list(NULL, "hello"))
    expect_equal(x, x0)
})


test_that("'term_maxtrix' weights works", {
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    w <- c(2, 3, 1)
    x0 <- term_matrix(text)
    x <- term_matrix(text, weights = w)
    expect_equal(x, w * x0)
})


test_that("'term_maxtrix' group works", {
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    g <- c("A", "B", "A")
    x0 <- term_matrix(text)
    x <- term_matrix(text, group = g)
    gmat <- Matrix::sparseMatrix(i = as.integer(factor(g)),
                                 j = seq_along(g), x = 1,
                                 dimnames = list(levels(factor(g)), NULL))
    expect_equal(x,  gmat %*% x0)
})


test_that("'term_matrix' weights and group works", {
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    w <- c(100, 3, 1000)
    g <- c("B", "B", "A")
    x0 <- term_matrix(text)
    x <- term_matrix(text, weights = w, group = g)
    gmat <- Matrix::sparseMatrix(i = as.integer(factor(g)),
                                 j = seq_along(g), x = 1,
                                 dimnames = list(levels(factor(g)), NULL))
    expect_equal(x,  gmat %*% (w * x0))
})

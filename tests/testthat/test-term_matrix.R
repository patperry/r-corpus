context("term_matrix")

test_that("'term_matrix' works", {
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    x <- term_matrix(text)

    # compute the term matrix by hand
    toks <- text_tokens(text)
    i <- rep(seq_along(toks), sapply(toks, length))
    toks <- factor(c(toks, recursive = TRUE))
    j <- as.integer(toks)
    x0 <- Matrix::sparseMatrix(i[!is.na(j)], j[!is.na(j)], x = 1,
                               dimnames=list(NULL, levels(toks)))

    # dimensions should agree
    expect_equal(dim(x), dim(x0))

    # column names should agree (but not necessarily be in same order)
    expect_equal(sort(colnames(x)), sort(colnames(x0)))
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
    expect_equal(sort(colnames(x)), sort(colnames(x0)))
    expect_equal(x[, colnames(x0), drop = FALSE],  gmat %*% x0)
})


test_that("'term_maxtrix' group can handle NA", {
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    g <- c("A", NA, "B")
    x0 <- term_matrix(c(A=text[1], B=text[3]))
    x <- term_matrix(text, group = g)
    expect_equal(x, x0)
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
    expect_equal(sort(colnames(x)), sort(colnames(x0)))
    expect_equal(x[, colnames(x0), drop = FALSE],  gmat %*% (w * x0))
})


test_that("'term_matrix' can transpose", {
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    w <- c(100, 3, 1000)
    g <- c("B", "B", "A")
    x <- term_matrix(text, weights = w, group = g, transpose = TRUE)
    x0 <- t(as.matrix(term_matrix(text, weights = w, group = g)))
    expect_equal(as.matrix(x), x0)
})


test_that("'term_matrix can select ngrams", {
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    ngrams <- c(1, 3)
    x <- term_matrix(text, ngrams = ngrams)
    terms <- colnames(x)
    for (i in seq_len(nrow(x))) {
        stats <- term_counts(text[i], ngrams = ngrams)
        expect_true(all(stats$term %in% terms))
        expect_equal(as.numeric(x[i,stats$term]), stats$count)
        expect_true(all(x[i,!terms %in% stats$term] == 0))
    }
})


test_that("'term_matrix' can select stemmed bigrams", {
    text <- "A sentence. Another sentence. Others..."
    f <- token_filter(stemmer = "english", drop_punct = TRUE,
                      drop = stopwords("english"))
    select <- c("sentenc", "anoth", "anoth sentenc")
    x0 <- term_matrix(text, f, select = select)
    x <- Matrix::sparseMatrix(i = c(1, 1, 1),
                              j = c(1, 2, 3),
                              x = c(2, 1, 1),
                              dimnames = list(NULL, select))
    expect_equal(x, x0)
})


test_that("'term_matrix' errors for empty terms", {
    expect_error(term_matrix("", select = c("a", "b", " ", "c")),
                 "select term in position 3 (' ') does not contain a type",
                 fixed = TRUE)
})


test_that("'term_matrix' errors for dropped select terms", {
    f <- token_filter(drop = "a")
    expect_error(term_matrix("", f, select = c("b b", "b a", "c")),
                 paste0("select term in position 2 ('b a')",
                        " contains a dropped type ('a')"),
                 fixed = TRUE)
})


test_that("'term_matrix' errors for duplicated select terms", {
    expect_error(term_matrix("", select = c("a", "b", "c", "b")),
                 paste0("select terms in positions 2 and 4 ('b' and 'b')",
                        " have the same type"),
                 fixed = TRUE)
})


test_that("'term_matrix' can select really long terms", {
    term <- paste(rep(letters, 4), collapse = " ")
    TERM <- paste(rep(LETTERS, 4), collapse = " ")
    x0 <- term_matrix(paste(term, TERM, term, term, TERM, sep = "|"),
                      select = term)
    x <- Matrix::sparseMatrix(i = 1, j = 1, x = 5,
                              dimnames = list(NULL, term))
    expect_equal(x, x0)
})


test_that("'term_matrix' can select types ending in '.s", {
    f <- token_filter(stemmer = "english", drop_punct = TRUE, combine = NULL)
    expect_equal(term_matrix("u.s.", f, select = "u.s"),
                 Matrix::sparseMatrix(i = 1, j = 1, x = 1,
                                      dimnames = list(NULL, "u.s")))
})


test_that("'term_frame' gives equivalent results to 'term_matrix'", {
    text <- c(a="A rose is a rose is a rose.",
              b="A Rose is red, a violet is blue!",
              c="A rose by any other name would smell as sweet.")
    x <- term_matrix(text)
    tf <- term_frame(text)
    xtf <- Matrix::sparseMatrix(i = as.integer(tf$text),
                                j = as.integer(tf$term),
                                x = tf$count,
                                dimnames = list(levels(tf$text),
                                                levels(tf$term)))
    expect_equal(x, xtf)
})


test_that("'term_frame' gives equivalent results to 'term_matrix' no names", {
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    x <- term_matrix(text)
    rownames(x) <- as.character(seq_along(text))
    tf <- term_frame(text)
    xtf <- Matrix::sparseMatrix(i = as.integer(tf$text),
                                j = as.integer(tf$term),
                                x = tf$count,
                                dimnames = list(levels(tf$text),
                                                levels(tf$term)))
    expect_equal(x, xtf)
})


test_that("'term_frame' can return character columns", {
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    tf <- term_frame(text)
    tf2 <- term_frame(text, factors = FALSE)
    expect_equal(tf2$text, as.character(tf$text))
    expect_equal(tf2$term, as.character(tf$term))
})


test_that("'term_frame' with group gives equivalent results to 'term_matrix'", {
    text <- c(a="A rose is a rose is a rose.",
              b="A Rose is red, a violet is blue!",
              c="A rose by any other name would smell as sweet.")
    g <- factor(c("X", "Y", "X"))
    x <- term_matrix(text, group = g)
    tf <- term_frame(text, group = g)
    xtf <- Matrix::sparseMatrix(i = as.integer(tf$group),
                                j = as.integer(tf$term),
                                x = tf$count,
                                dimnames = list(levels(tf$group),
                                                levels(tf$term)))
    expect_equal(x, xtf)
})

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


test_that("'term_matrix' can transpose", {
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    g <- c("B", "B", "A")
    x <- term_matrix(text, group = g, transpose = TRUE)
    x0 <- t(Matrix::as.matrix(term_matrix(text, group = g)))
    expect_equal(Matrix::as.matrix(x), x0)
})


test_that("'term_matrix can select ngrams", {
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    ngrams <- c(1, 3)
    x <- term_matrix(text, ngrams = ngrams)
    terms <- colnames(x)
    for (i in seq_len(nrow(x))) {
        stats <- term_stats(text[i], ngrams = ngrams)
        expect_true(all(stats$term %in% terms))
        expect_equal(as.numeric(x[i,stats$term]), stats$count)
        expect_true(all(x[i,!terms %in% stats$term] == 0))
    }
})


test_that("'term_matrix' can select stemmed bigrams", {
    text <- "A sentence. Another sentence. Others..."
    f <- text_filter(stemmer = "english", drop_punct = TRUE,
                     drop = stopwords_en)
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
                 "select term in position 3 (\" \") has empty type (\"\")",
                 fixed = TRUE)
})


test_that("'term_matrix' errors for dropped select terms", {
    f <- text_filter(drop = "a")
    expect_error(term_matrix("", f, select = c("b b", "a a", "c")),
                 paste0("select term in position 2 (\"a a\")",
                        " contains a dropped type (\"a\")"),
                 fixed = TRUE)
})


test_that("'term_matrix' errors for duplicated select terms", {
    expect_error(term_matrix("", select = c("a", "b", "c", "b")),
                 paste0("select terms in positions 2 and 4 (\"b\" and \"b\")",
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
    f <- text_filter(stemmer = "en", drop_punct = TRUE, combine = NULL)
    expect_equal(term_matrix("u.s.", f, select = "u."),
                 Matrix::sparseMatrix(i = 1, j = 1, x = 1,
                                      dimnames = list(NULL, "u.")))
})


test_that("'term_matrix' can handle mmapped JSON with escapes", {
    tmp <- tempfile()
    writeLines('{"text": "\\u00a3"}', tmp)
    on.exit(unlink(tmp))

    data <- read_ndjson(tmp, mmap = TRUE, text = "text")
    x <- term_matrix(data)
    expect_equal(colnames(x), "\u00a3")
})

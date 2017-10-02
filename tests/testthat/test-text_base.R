context("text_base")

test_that("invalid ops should fail", {
    x <- as_corpus_text("hello")
    expect_error(Re(x), "'Re' is not defined for text objects")
    expect_error(abs(x), "'abs' is not defined for text objects")
    expect_error(!x, "unary ! is not defined for text objects")
    expect_error(x + x, "+ is not defined for text objects")
    expect_error(all(x), "'all' is not defined for text objects")
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


test_that("as.vector works", {
    x <- as_corpus_text(letters)
    names(x) <- LETTERS

    expect_equal(as.vector(x), as.character(x))
    expect_equal(as.vector(x, mode = "list"),
                 as.vector(as.character(x), mode = "list"))
})


test_that("all.equal fails if arg has wrong type", {
    expect_error(all.equal.corpus_text(letters, letters),
                 "'target' is not a valid text object")
})

test_that("all.equal tests if class is wrong", {
    expect_equal(all.equal(as_corpus_text(letters), letters),
                 "target is corpus_text, current is character")
})


test_that("all.equal tests if names are wrong", {
    x <- as_corpus_text(letters, names = LETTERS)
    y <- as_corpus_text(letters)
    expect_equal(all.equal(x, y), "names for target but not for current")
    expect_equal(all.equal(y, x), "names for current but not for target")

    z <- as_corpus_text(x, names = c("a", LETTERS[-1]))
    expect_equal(all.equal(x, z), "Names: 1 string mismatch")
})


test_that("all.equal tests if class is wrong", {
    x <- as_corpus_text(letters)
    y <- as_corpus_text(letters)
    class(y) <- c("my_text", "corpus_text")
    expect_false(isTRUE(all.equal(x, y)))
})


test_that("all.equal tests if filter is wrong", {
    x <- as_corpus_text(letters)
    y <- as_corpus_text(letters, filter = text_filter(map_case = FALSE))
    z <- as_corpus_text(letters, filter = text_filter())
    expect_false(isTRUE(all.equal(x, y)))
    expect_true(isTRUE(all.equal(x, z)))
})


test_that("all.equal returns TRUE if both are NULL", {
    expect_true(all.equal.corpus_text(NULL, NULL))
})


test_that("as.Date works", {
    x <- c("1981-10-26", "2000-01-01", NA)
    expect_equal(as.Date(as_corpus_text(x)), as.Date(x))
})


test_that("as.data.frame works", {
    text <- as_corpus_text(letters)
    expect_equal(as.data.frame(text),
                 data.frame(text))
})


test_that("'summary' works", {
    skip_if_not(with(R.Version(), paste(major, minor, sep = ".")) >= "3.4.0",
                "'summary.integer' rounds to integer on R < 3.4.0")

    text <- as_corpus_text(federalist$text)
    value <- summary(text)
    ntok <- text_ntoken(text)
    expect_equal(summary(text),
                 structure(c("Min. Toks." = min(ntok),
                             "1st Qu. Toks." = quantile(ntok, .25)[[1]],
                             "Median Toks." = median(ntok),
                             "Mean Toks." = mean(ntok),
                             "3rd Qu. Toks." = quantile(ntok, .75)[[1]],
                             "Max. Toks." = max(ntok),
                             "Types" = text_ntype(text, collapse = TRUE)),
                           class = c("summaryDefault", "table")))
})


test_that("'summary' handles NA", {
    skip_if_not(with(R.Version(), paste(major, minor, sep = ".")) >= "3.4.0",
                "'summary.integer' rounds to integer on R < 3.4.0")

    text <- as_corpus_text(federalist$text)
    text[c(5, 13, 40, 55)] <- NA
    value <- summary(text)
    ntok <- text_ntoken(text[!is.na(text)])
    expect_equal(summary(text),
                 structure(c("Min. Toks." = min(ntok),
                             "1st Qu. Toks." = quantile(ntok, .25)[[1]],
                             "Median Toks." = median(ntok),
                             "Mean Toks." = mean(ntok),
                             "3rd Qu. Toks." = quantile(ntok, .75)[[1]],
                             "Max. Toks." = max(ntok),
                             "Types" = text_ntype(text, collapse = TRUE),
                             "NA's" = 4),
                           class = c("summaryDefault", "table")))
})


test_that("'summary' hanldes empty", {
    expect_equal(summary(as_corpus_text(character())),
                 structure(c("Min. Toks." = NA,
                             "1st Qu. Toks." = NA,
                             "Median Toks." = NA,
                             "Mean Toks." = NaN,
                             "3rd Qu. Toks." = NA,
                             "Max. Toks." = NA,
                             "Types" = 0),
                           class = c("summaryDefault", "table")))
})

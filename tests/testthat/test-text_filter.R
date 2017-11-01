context("text_filter")


test_that("'text_filter' has the right defaults", {
    f <- text_filter()
    expect_equal(f$map_case, TRUE)
    expect_equal(f$map_quote, TRUE)
    expect_equal(f$remove_ignorable, TRUE)
    expect_equal(f$stemmer, NULL)
    expect_equal(f$stem_dropped, FALSE)
    expect_equal(f$stem_except, NULL)
    expect_equal(f$combine, NULL)
    expect_equal(f$drop_letter, FALSE)
    expect_equal(f$drop_number, FALSE)
    expect_equal(f$drop_punct, FALSE)
    expect_equal(f$drop_symbol, FALSE)
    expect_equal(f$drop, NULL)
    expect_equal(f$drop_except, NULL)
    expect_equal(f$sent_crlf, FALSE)
    expect_equal(f$sent_suppress, abbreviations_en)
})


test_that("'text_filter' has the same defaults for all objects", {
    x <- c("hello", "world", "how", "are", "you?")
    y <- as_corpus_text(x)
    z <- data.frame(text = x)
    w <- data.frame(text = y)
    expect_equal(text_filter(x), text_filter())
    expect_equal(text_filter(y), text_filter())
    expect_equal(text_filter(z), text_filter())
    expect_equal(text_filter(w), text_filter())
})


test_that("'text_filter' can be assigned to text", {
    x <- as_corpus_text("hello")
    f0 <- text_filter(x)
    d <- data.frame(text = x)
    f <- text_filter(map_case = FALSE)
    text_filter(x) <- f
    expect_equal(text_filter(x), f)
    expect_equal(text_filter(d), f0)
})


test_that("'text_filter' can be assigned to data frame with \"text\" column", {
    x <- as_corpus_text("hello")
    f0 <- text_filter(x)
    d <- data.frame(text = x)
    f <- text_filter(map_case = FALSE)
    text_filter(d) <- f
    expect_equal(text_filter(x), f0)
    expect_equal(text_filter(d), f)
})


test_that("'text_filter' fails without data frame with \"text\" column", {
    x <- as_corpus_text("hello")
    d <- data.frame(not_text = x)
    f <- text_filter(map_case = FALSE)
    expect_error(text_filter(d), "no column named \"text\" in data frame")
    expect_error(text_filter(d) <- f, "no column named \"text\" in data frame")
})


test_that("'text_filter' cannot be assigned to character", {
    x <- "hello"
    d <- data.frame(text = x, stringsAsFactors = FALSE)
    f <- text_filter(map_case = FALSE)
    expect_error(text_filter(x) <- f,
                 "setting a text filter for objects of class \"character\" is not allowed",
                 fixed = TRUE)
    expect_error(text_filter(d) <- f,
                 "setting a text filter for objects of class \"character\" is not allowed",
                 fixed = TRUE)
})


test_that("setting an unrecognized property gives an error", {
    f <- text_filter()
    expect_error(f$foo <- "bar",
                 "unrecognized text filter property: 'foo'",
                 fixed = TRUE)

    expect_error(text_filter(foo = "bar"),
                 "unrecognized text filter property: 'foo'",
                 fixed = TRUE)
})


test_that("passing unnamed arguments is not allowed", {
    expect_error(text_filter(NULL, TRUE),
                 "unnamed arguments are not allowed")

    x <- as_corpus_text("hello")
    expect_error(text_filter(x, TRUE),
                 "unnamed arguments are not allowed")
})


test_that("giving invalid text to text_filter.corpus_text is not allowed", {
    expect_error(text_filter.corpus_text("hello"),
                 "argument is not a valid text object")
})


test_that("'as_corpus_text' propagates a non-NULL filter argument to character", {
    x <- "hello"
    f <- text_filter(map_case = FALSE)
    x1 <- as_corpus_text("hello", filter = f)
    x2 <- as_corpus_text("hello")
    x3 <- as_corpus_text("world", filter = f)
    expect_false(isTRUE(all.equal(x1, x2)))
    expect_false(isTRUE(all.equal(x1, x3)))
    expect_equal(text_filter(x1), f)
})


test_that("'as_corpus_text' propagates a non-NULL to text filter to text", {
    x <- as_corpus_text("hello")
    f0 <- text_filter(map_case = FALSE, map_quote = FALSE)
    text_filter(x) <- f0
    expect_equal(text_filter(x), f0)

    f1 <- text_filter(map_case = TRUE, map_quote = FALSE)
    y <- as_corpus_text(x, filter = f1)
    expect_equal(text_filter(y), f1)
})


test_that("'as_corpus_text' propagates a non-NULL to text filter to data frame", {
    d <- data.frame(text = "hello")
    f0 <- text_filter(d)

    f <- text_filter(map_case = FALSE)
    x <- as_corpus_text(d, filter = f)
    expect_equal(text_filter(d), f0)
    expect_equal(text_filter(x), f)
})


test_that("'as_corpus_text' propagates a non-NULL to filter to text data frame", {
    d <- data.frame(text = as_corpus_text("hello"))
    f0 <- text_filter(d)

    f <- text_filter(map_case = FALSE)
    x <- as_corpus_text(d, filter = f)
    expect_equal(text_filter(d), f0)
    expect_equal(text_filter(x), f)
})


test_that("'as_corpus_text' with NULL filter leaves it unchanged", {
    x <- as_corpus_text("hello")
    f0 <- text_filter(map_case = FALSE, map_quote = FALSE)
    text_filter(x) <- f0

    y <- as_corpus_text(x, filter = NULL)
    expect_equal(text_filter(y), f0)
})


test_that("'text_filter' clears the old filter", {
    x <- as_corpus_text("wicked")
    y <- as_corpus_text(x, filter = text_filter(stemmer = "english"))

    toks1 <- text_tokens(y)
    expect_equal(text_tokens(y), list("wick"))

    toks2 <- text_tokens(x)
    expect_equal(toks2, list("wicked"))
})


test_that("'text_filter' can override properties", {
    x <- as_corpus_text("hello", remove_ignorable = FALSE)
    f <- text_filter(x, map_case = FALSE, stemmer = "english")
    f2 <- text_filter(remove_ignorable = FALSE, map_case = FALSE,
                      stemmer = "english")
    expect_equal(f, f2)
})


test_that("'text_filter<-' rejects invalid inputs", {
    x <- "hello"
    expect_error(`text_filter<-.corpus_text`(x, text_filter()),
                 "argument is not a valid text object")
})


test_that("'text_filter<-' setting NULL works", {
    x <- as_corpus_text("hello")
    f <- text_filter(x)
    text_filter(x) <- NULL
    f2 <- text_filter(x)
    expect_equal(f, f2)
})


test_that("setting invalid text_filter properties fails", {
    f <- text_filter()
    expect_error(f$map_ca <- TRUE,
                 "unrecognized text filter property: 'map_ca'")

    expect_error(f[[c(1,1)]] <- TRUE, "no such text filter property")
    expect_error(f[[0]] <- TRUE, "no such text filter property")
    expect_error(f[[length(f) + 1]] <- TRUE, "no such text filter property")
    expect_error(f[[NA]] <- TRUE, "no such text filter property")
})


test_that("setting numeric properties succeeds", {
    f <- text_filter()
    i <- match("combine", names(f))
    f[[i]] <- "new york city"
    expect_equal(f$combine, "new york city")
})


test_that("setting multiple properties works", {
    f <- text_filter()

    f[c("map_case", "map_quote")] <- FALSE
    expect_equal(f, text_filter(map_case = FALSE, map_quote = FALSE))

    f[c("map_case", "remove_ignorable", "map_quote")] <- c(FALSE, FALSE, TRUE)
    expect_equal(f, text_filter(map_case = FALSE, map_quote = TRUE,
                                remove_ignorable = FALSE))
})


test_that("invalid operations send errors", {
    f <- text_filter()
    expect_error(f[c(NA, "map_case", NA)] <- FALSE,
                 "NAs are not allowed in subscripted assignments")
    expect_error(f[c(-1, 2)] <- FALSE,
                 "only 0's may be mixed with negative subscripts")
    expect_error(f[100] <- "hello",
                 "no such text filter property")
    expect_error(f["map_case"] <- c(TRUE, FALSE),
                 "number of items to replace differs from the replacement length")
    expect_error(f[c("map_case", "map_case", "map_quote")] <- c(TRUE, FALSE),
                 "number of items to replace differs from the replacement length")
})


test_that("text filter printing works", {
    f <- text_filter()
expected <- c(
'Text filter with the following options:',
'',
'    map_case: TRUE',
'    map_quote: TRUE',
'    remove_ignorable: TRUE',
'    combine: NULL',
'    stemmer: NULL',
'    stem_dropped: FALSE',
'    stem_except: NULL',
'    drop_letter: FALSE',
'    drop_number: FALSE',
'    drop_punct: FALSE',
'    drop_symbol: FALSE',
'    drop: NULL',
'    drop_except: NULL',
'    connector: _',
'    sent_crlf: FALSE',
'    sent_suppress:  chr [1:155] "A." "A.D." "a.m." "A.M." "A.S." "AA." ...')

    skip_if_not(with(R.Version(), paste(major, minor, sep = "."))
                >= "3.4.0", "str output changed on R 3.4.0")

    actual <- strsplit(capture_output(print(f), width = 80), "\n")[[1]]
    expect_equal(actual, expected)
})

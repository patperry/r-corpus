context("text")

test_that("converting to character should work", {
    x <- c("hello", NA, "world", "")
    y <- as_text(x)
    expect_equal(as.character(y), x)
})


test_that("subsetting should work", {
    x <- as_text(LETTERS)
    i <- c(7, 2, 3, 21, 15)
    y <- x[i]
    expect_equal(y, as_text(LETTERS[i]));
})


test_that("`format` should handle NAs", {
    x <- c(NA, "Friday, November 23, 1787", NA)
    expect_equal(format(as_text(x), na.print = "NA"),
                 format(as.character(x)))
})


test_that("`as.data.frame` should work", {
    x <- as_text(c(a="1", b="2", c="foo", d="bar"))
    d <- as.data.frame(x)

    expect_equal(nrow(d), length(x))
    expect_equal(names(d), "x")
    expect_equal(rownames(d), names(x))
    expect_equal(d[["x"]], as_text(c("1", "2", "foo", "bar"))) # drop names
})


test_that("serialization should work", {
    x <- c("Once upon a time there were four little Rabbits,",
	       "and their names were: Flopsy, Mopsy, Cottontail, and Peter.",
	       "They lived with their Mother in a sandbank,",
	       "underneath the root of a very big fir tree.",
	       "'Now, my dears,' said old Mrs. Rabbit one morning,",
	       "'you may go into the fields or down the lane,",
	       "but don't go into Mr. McGregor's garden --",
	       "your Father had an accident there;",
	       "he was put in a pie by Mrs. McGregor.'")

    file <- tempfile()
    writeLines(paste0('{"text": "', x, '"}'), file)
    ds <- read_ndjson(file)
    text <- as_text(ds)

    file2 <- tempfile()
    saveRDS(text, file2)
    text2 <- readRDS(file2)

    expect_equal(text, text2)
})


test_that("c should work", {
    x <- c(a=as_text(c("hello", NA, "world")), "!", c=7)
    expect_equal(x, as_text(c(a1 = "hello", a2 = NA, a3 = "world", "!",
                              c = "7")))
})


test_that("rep should work", {
    x <- as_text(c("a", "b", "c"))
    y <- rep(x, 7)
    expect_equal(y, as_text(rep(c("a", "b", "c"), 7)))
})


test_that("is.na should work", {
    x <- as_text(c("a", NA, "", "b"))
    expect_equal(is.na(x), c(FALSE, TRUE, FALSE, FALSE))
    expect_equal(is.na(as_text(c())), logical())
})


test_that("anyNA should work", {
    x <- as_text(c("a", NA, "", "b"))
    y <- as_text(c())
    z <- as_text(letters)

    expect_true(anyNA(x))
    expect_false(anyNA(y))
    expect_false(anyNA(z))
})


test_that("conversions should work", {
    expect_equal(as.complex(as_text("1+2i")), 1+2i)
    expect_equal(as.double(as_text("3.14")), 3.14)
    expect_equal(as.integer(as_text("3.14")), 3)
    expect_equal(as.logical(as_text(c("TRUE", "FALSE", "NA"))),
                 c(TRUE, FALSE, NA))
    expect_equal(as.numeric(as_text("3.14")), 3.14)
    expect_equal(as.raw(as_text("1")), as.raw("1"))

    expect_warning(x <- as.numeric(as_text("foo")),
                   "NAs introduced by coercion")
    expect_equal(x, NA_real_)
})


test_that("invalid operations should error", {
    x <- as_text("hello")
    expect_error(x$names, "$ operator is invalid for text objects",
                 fixed = TRUE)
    expect_error(x$names <- "foo", "$<- operator is invalid for text objects",
                 fixed = TRUE)
    expect_error(as.environment(x),
                 "'as.environment' is invalid for text objects")
})


test_that("text methods should error for non-text", {
    expect_error(names.corpus_text("hello"), "invalid text object")
    expect_error(`[.corpus_text`("hello", 1), "invalid text object")
})

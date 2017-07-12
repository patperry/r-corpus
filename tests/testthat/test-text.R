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

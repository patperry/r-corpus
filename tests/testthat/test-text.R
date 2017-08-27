context("text")


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


test_that("conversion to and from character should work", {
    x <- c("Once upon a time there were four little Rabbits,",
	       "and their names were: Flopsy, Mopsy, Cottontail, and Peter.",
	       "They lived with their Mother in a sandbank,",
	       "underneath the root of a very big fir tree.",
	       "'Now, my dears,' said old Mrs. Rabbit one morning,",
	       "'you may go into the fields or down the lane,",
	       "but don't go into Mr. McGregor's garden --")
    y <- c("your Father had an accident there;",
           NA_character_,
	       "he was put in a pie by Mrs. McGregor.'")

    text_x <- as_text(x)
    text_y <- as_text(y)

    expect_equal(as.character(text_x), x)
    expect_equal(as.character(text_y), y)

    expect_equal(as.character(c(text_x, text_y)), c(x, y))

    expect_equal(as.character(text_x[c(2,4,3,2)]), x[c(2,4,3,2)])

    i <- c(5, 9, 2, 6, 3)
    expect_equal(as.character(c(text_x, text_y)[i]), c(x, y)[i])
})

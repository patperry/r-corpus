context("dataset_record")


test_that("converting to list works", {
    x <- as.integer(c(1, 1, 2, 3, 5))
    y <- c("F", "i", "b", "b", "o")
    file <- tempfile()
    writeLines(paste0('{"x":', x, ',"y":"', y, '"}'), file)

    ds <- read_ndjson(file)
    l <- as.list(ds)

    expect_equal(l, list(x=x, y=as_text(y)))

    rm("ds"); gc(); file.remove(file)
})

context("json_record")


test_that("converting to list works", {
    x <- as.integer(c(1, 1, 2, 3, 5))
    y <- c("F", "i", "b", "b", "o")
    file <- tempfile()
    writeLines(paste0('{"x":', x, ',"y":"', y, '"}'), file)

    ds <- read_ndjson(file, stringsAsFactors = FALSE)
    l <- as.list(ds)

    expect_equal(l, list(x=x, y=y))
})


test_that("printing works", {
    file <- tempfile()
    writeLines(c('{"a": 1, "b": true, "c": [3.14]}',
                 '{"b": false, "c":[2.4, -1.0], "d": "hello"}'),
               file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(capture_output(print(ds)),
        paste('JSON data set with 2 rows of the following type:',
              '{',
              '\t"a": integer,',
              '\t"b": boolean,',
              '\t"c": [real],',
              '\t"d": text',
              '}', sep = '\n'))
})


test_that("subscripting both dimensions works", {
    file <- tempfile()
    writeLines(c('{"a": 1, "b": true, "c": [3.14]}',
                 '{"b": false, "c":[2.4, -1.0], "d": "hello"}'),
               file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_error(ds[2, c("c", "a")],
                 "second subscript of length >1 is not allowed")

    expect_equal(as.list(ds[2, "c"]), list(c(2.4, -1.0)))
    expect_equal(as.integer(ds[2, "a"]), NA_integer_)
    expect_equal(as.list(ds[,]), as.list(ds))
    expect_equal(as.list(ds[]), as.list(ds))
})

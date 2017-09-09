context("json_record")


test_that("converting to list works", {
    x <- as.integer(c(1, 1, 2, 3, 5))
    y <- c("F", "i", "b", "b", "o")
    file <- tempfile()
    writeLines(paste0('{"x":', x, ',"y":"', y, '"}'), file)

    ds <- read_ndjson(file)
    l <- as.list(ds)

    expect_equal(l, list(x=x, y=y))
})


test_that("decoding record any works", {
    file <- tempfile()
    writeLines(c('{"b": false}',
                 '{"b": 7}'), file)
    ds <- read_ndjson(file)
    expect_equal(ds, structure(list(b = list(FALSE, 7)),
                               row.names = c(NA, -2),
                               class = c("corpus_frame", "data.frame")))
})


test_that("converting to list with null works", {
    file <- tempfile()
    writeLines(c('{"a": 1, "b": false}',
                 'null',
                 '{"b": 7, "c": true}'), file)
    ds <- read_ndjson(file)
    expect_equal(ds, structure(list(a = c(1, NA, NA),
                                    b = list(FALSE, NULL, 7),
                                    c = c(NA, NA, TRUE)),
                               row.names = c(NA, -3),
                               class = c("corpus_frame", "data.frame")))
})


test_that("length works", {
    x <- as.integer(c(1, 1, 2, 3, 5))
    y <- c("F", "i", "b", "b", "o")
    file <- tempfile()
    writeLines(paste0('{"x":', x, ',"y":"', y, '"}'), file)

    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(length(ds), 2)
})


test_that("dimnames works", {
    x <- as.integer(c(1, 1, 2, 3, 5))
    y <- c("F", "i", "b", "b", "o")
    file <- tempfile()
    writeLines(paste0('{"x":', x, ',"y":"', y, '"}'), file)

    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(dimnames(ds), list(as.character(1:5), c("x", "y")))
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

    expect_equal(as.list(ds[2, "c", drop = FALSE]), list(c(2.4, -1.0)))
    expect_equal(as.integer(ds[2, "a"]), NA_integer_)
    expect_equal(as.list(ds[,]), as.list(ds))
    expect_equal(as.list(ds[]), as.list(ds))
    expect_equal(as.logical(ds[,"b"]), c(TRUE, FALSE))
})


test_that("deserializing as text works", {
    file <- tempfile()
    writeLines('{"a": "hello", "b": "world"}', file)
    ds <- read_ndjson(file, text = "b")
    expect_equal(ds$a, "hello")
    expect_equal(ds$b, as_corpus_text("world"))
})


test_that("decoding nested records works", {
    lines <- c('{ "a": 1, "b": true }',
               '{ "b": false, "nested": { "c": 100, "d": false }}',
               '{ "a": 3.14, "nested": { "d": true }}')
    file <- tempfile()
    writeLines(lines, file)
    ds <- read_ndjson(file)

    expect_equal(ds,
                 structure(list(a = c(1, NA, 3.14),
                                b = c(TRUE, FALSE, NA),
                                nested.c = c(NA, 100, NA),
                                nested.d = c(NA, FALSE, TRUE)),
                           row.names = c(NA, -3),
                           class = c("corpus_frame", "data.frame")))
})


test_that("converting to text works", {
    file <- tempfile()
    writeLines('{ "a": "foo", "text": "hello" }', file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(as_corpus_text(ds), as_corpus_text("hello"))
})


test_that("converting to text fails if no column named text", {
    file <- tempfile()
    writeLines('{ "a": "foo", "b": "hello" }', file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_error(as_corpus_text(ds), "no column named \"text\" in JSON object",
                 fixed = TRUE)
})


test_that("converting to text throws error if no column named 'text'", {
    file <- tempfile()
    writeLines('{ "a": "foo", "b": "hello" }', file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_error(as_corpus_text(ds), 'no column named "text"')
})


test_that("decoding text or character as requested works", {
    file <- tempfile()
    writeLines('{ "a": "foo", "b": "bar", "c": "baz", "d": "boo" }', file)
    ds <- read_ndjson(file, text = c("b", "c"))
    expect_equal(ds$a, "foo")
    expect_equal(ds$b, as_corpus_text("bar"))
    expect_equal(ds$c, as_corpus_text("baz"))
    expect_equal(ds$d, "boo")
})


test_that("decoding text or character as requested works not simplifying", {
    file <- tempfile()
    writeLines('{ "a": "foo", "b": "bar", "c": "baz", "d": "boo" }', file)
    ds <- read_ndjson(file, text = c("b", "c"), simplify = FALSE)
    expect_equal(ds$a, "foo")
    expect_equal(ds$b, as_corpus_text("bar"))
    expect_equal(ds$c, as_corpus_text("baz"))
    expect_equal(ds$d, "boo")
})


test_that("invalid operations don't work", {
    lines <- c('{ "a": 1, "b": true }',
               '{ "b": false, "nested": { "c": 100, "d": false }}',
               '{ "a": 3.14, "nested": { "d": true }}')
    file <- tempfile()
    writeLines(lines, file)
    ds <- read_ndjson(file, simplify = FALSE)

    expect_error(names(ds) <- c("a", "b", "c"),
                 "setting names on a JSON object is not allowed")
    expect_error(ds$a <- c(1, 2, 3),
                 "$<- operator is invalid for JSON objects", fixed = TRUE)
    expect_error(ds[[1]] <- c(1, 2, 3),
                 "[[<- operator is invalid for JSON objects", fixed = TRUE)
    expect_error(ds[1,1] <- 1,
                 "[<- operator is invalid for JSON objects", fixed = TRUE)

    expect_error(ds[[c()]], "subscript of length 0 is not allowed")
    expect_error(ds[[c(1,2)]], "subscript of length >1 is not allowed")
    expect_error(ds[["foo"]], "invalid column name: \"foo\"")
    expect_error(ds[[NA]], "invalid subscript: \"NA\"")
    expect_error(ds[1, "foo"], "invalid column name: \"foo\"")
    expect_error(ds[1, NA], "invalid column subscript: \"NA\"")
    expect_error(ds[1, 100], "invalid column subscript: \"100\"")
    expect_error(ds[1, NULL], "second subscript of length 0 is not allowed")
    expect_error(ds[1, integer()],
                 "second subscript of length 0 is not allowed")

    expect_error(ds[1, 1, 1], "'drop' must be TRUE or FALSE")

    expect_error(`[.corpus_json`(data.frame(x=1,y=2), 1, 1),
                 "invalid JSON object")
})

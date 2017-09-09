context("json_scalar")


test_that("reading integer works", {
    x <- c(48, -18, 42, 50, -4, 28, -18, -26, 11)
    file <- tempfile()
    writeLines(as.character(x), file)
    ds <- read_ndjson(file, simplify = FALSE)

    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as.integer(ds), x)
})


test_that("subscripting and subsetting integer works", {
    x <- c(-194, 273, -112, -76, 38, 0, 0, 99)
    file <- tempfile()
    writeLines(as.character(x), file)
    ds <- read_ndjson(file, simplify = FALSE)

    for (i in seq_along(data)) {
        expect_equal(ds[[i]], x[[i]])
    }

    expect_equal(as.integer(ds[c(1, 7, 3)]), x[c(1, 7, 3)])
    expect_equal(as.integer(ds[c()]), x[c()])
})


test_that("reading double works", {
    x <- c(-1.6571900, 0.1579793, -1.7205961, 0.1625917, 0.3143757, -1.1230602)
    file <- tempfile()
    writeLines(as.character(x), file)
    ds <- read_ndjson(file, simplify = FALSE)

    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as.numeric(ds), x)
})


test_that("subscripting and subsetting double works", {
    x <- c(-1.6571900, 0.1579793, -1.7205961, 0.1625917, 0.3143757, -1.1230602)
    file <- tempfile()
    writeLines(as.character(x), file)
    ds <- read_ndjson(file, simplify = FALSE)

    for (i in seq_along(data)) {
        expect_equal(ds[[i]], x[[i]])
    }

    expect_equal(as.numeric(ds[c(2, 4)]), x[c(2, 4)])
})


test_that("reading text works", {
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
    writeLines(paste0('"', x, '"'), file)
    ds <- read_ndjson(file, simplify = FALSE)

    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as_corpus_text(ds), as_corpus_text(x))
    expect_equal(as.character(ds), x)
})


test_that("reading boolean works", {

    x <- c(TRUE, FALSE, NA, TRUE, TRUE, FALSE, NA)
    file <- tempfile()
    writeLines(ifelse(is.na(x), "null", ifelse(x, "true", "false")), file)
    ds <- read_ndjson(file, simplify = FALSE)

    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as.logical(ds), x)
})


test_that("reading empty works", {
    file <- tempfile()
    writeLines(character(), file)
    ds <- read_ndjson(file, simplify = FALSE)

    expect_equal(length(ds), 0)
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
})


test_that("reading double array works", {
    x <- list(3.14, c(1, 2, 3), numeric(), c(5.6, -3.0),
              c(2.18, 0.0028, 1e99, -2.1e12))
    file <- tempfile()
    writeLines(sapply(x, function(xi)
                      paste0("[", paste0(xi, collapse=", "), "]")), file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as.list(ds), x)
})


test_that("reading double array with null works", {
    file <- tempfile()
    writeLines("[1.2, null]", file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(as.list(ds), list(c(1.2, NA)))
})


test_that("reading double array with overflow works", {
    file <- tempfile()
    writeLines(c("[1e999999, -1e9999999]", "[0]"), file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_warning((y <- as.list(ds)),
                   "Inf introduced by coercion to double-precision range",
                   fixed = TRUE)
    expect_equal(y, list(c(Inf, -Inf), 0))
})


test_that("reading double vector with overflow works", {
    file <- tempfile()
    writeLines(c("1e999999", "-1e9999999", "null", "0"), file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_warning((y <- as.numeric(ds)),
                   "Inf introduced by coercion to double-precision range",
                   fixed = TRUE)
    expect_equal(y, c(Inf, -Inf, NA, 0))
})


test_that("reading double array with underflow works", {
    file <- tempfile()
    writeLines(c("[1e-999999, -1e-9999999]"), file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_warning((y <- as.list(ds)),
                   "0 introduced by coercion to double-precision range",
                   fixed = TRUE)
    expect_equal(y, list(c(0, -0)))
})


test_that("reading double vector with underflow works", {
    file <- tempfile()
    writeLines(c("1e-999999", "-1e-9999999", "null", "0"), file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_warning((y <- as.numeric(ds)),
                   "0 introduced by coercion to double-precision range",
                   fixed = TRUE)
    expect_equal(y, c(0, -0, NA, 0))
})


test_that("reading integer array works", {
    x <- list(c(4L,-1L,2L), integer(), integer(), c(1L, 1L, 2L, 3L, 5L))
    file <- tempfile()
    writeLines(sapply(x, function(xi)
                      paste0("[", paste0(xi, collapse=", "), "]")), file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as.list(ds), x)
})


test_that("reading integer array with null works", {
    file <- tempfile()
    writeLines("[1, null, -3]", file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(as.list(ds), list(c(1L, NA_integer_, -3L)))
})


test_that("reading integer array with overflow works", {
    file <- tempfile()
    writeLines(c("[-2147483648]", "[2147483648]"), file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(as.list(ds), list(-2147483648, 2147483648))
})


test_that("reading integer vector with overflow works", {
    file <- tempfile()
    writeLines(c("-2147483648", "-2147483647", "2147483647", "2147483648"),
               file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_warning(y <- as.integer(ds),
                   "NAs introduced by coercion to integer range")
    expect_equal(y, c(NA, -2147483647, 2147483647, NA))

    z <- read_ndjson(file, simplify = TRUE)
    expect_equal(z, c(-2147483648, -2147483647, 2147483647, 2147483648))
})


test_that("reading logical array works", {
    file <- tempfile()
    writeLines(c("[true, true, false]", "[]", "[false, null]", "null"), file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(length(ds), 4)
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as.list(ds), list(c(TRUE, TRUE, FALSE), logical(),
                                   c(FALSE, NA), NULL))
})


test_that("reading character array works", {
    x <- list(c("hello", "world"), "how", c("are", "you", "?"))
    file <- tempfile()
    writeLines(sapply(x, function(xi)
                      paste0('["', paste0(xi, collapse='", "'), '"]')), file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as.list(ds), x)
})


test_that("reading character array with null works", {
    file <- tempfile()
    writeLines(c('["hello", null, ""]', '[]', '[null]', 'null'), file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(as.list(ds), list(c("hello", NA, ""), character(),
                                   NA_character_, NULL))
})


test_that("reading record array works", {
    json <- c('[{"a": true, "b": false, "c": null}]',
              '[{"a": false, "c": true}]',
              '[]',
              'null',
              '[null]',
              '[{}]',
              '[{"a": true}, {"b": null}, {"c": true}]')
    x <- list(list(list(a = TRUE, b = FALSE, c = NA)),
              list(list(a = FALSE, c = TRUE)),
              list(),
              NULL,
              list(NULL),
              list(list()),
              list(list(a = TRUE), list(b = NA), list(c = TRUE)))

    file <- tempfile()
    writeLines(json, file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(as.list(ds), x)
    expect_equal(dimnames(ds), NULL)
})


test_that("reading heterogeneous array works", {
    file <- tempfile()
    writeLines(c('[true]', '[null]', '[1]', '[{"a":1}]',
                 '[{"a":1, "b":2}]'), file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(as.list(ds), list(list(TRUE), list(NULL), list(1),
                                   list(list(a=1)), list(list(a=1,b=2))))
})


test_that("reading heterogeneous works", {
    file <- tempfile()
    writeLines(c('true', 'null', '1', '{"a": 1}', '{"a":1, "b":2}'), file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(as.list(ds), list(TRUE, NULL, 1, list(a=1), list(a=1, b=2)))
})


test_that("reading array of heterogeneous array works", {
    file <- tempfile()
    writeLines(c('[[null, true, 1, 10000000000, 3.14, "hello"]]',
                 '[[1], [1,2,3]]'),
               file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(as.list(ds), list(list(list(NULL, TRUE, 1, 10000000000, 3.14,
                                             "hello")),
                                   list(list(1), list(1,2,3))))
})


test_that("reading string with escapes works", {
    file <- tempfile()
    writeLines(c('null', '"\\n"', '"\\u6025"', '"\\uD834\\uDD1E"'), file)
    ds <- read_ndjson(file)
    expect_equal(as.character(ds), c(NA, "\n", "\u6025", intToUtf8(0x1d11e)))
})


test_that("printing text works", {
    file <- tempfile()
    writeLines(c('"hello"', '"world"', '"how are you?"'),
               file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(capture_output(print(ds)),
        paste('JSON data set with 3 rows of type text'))
})


test_that("printing text array works", {
    file <- tempfile()
    writeLines(c('["hello"]', 'null', '["world"]', '["how", "are", "you?"]'),
               file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(capture_output(print(ds)),
        paste('JSON data set with 4 rows of type [text]'))
})


test_that("converting scalar to data frame works", {
    file <- tempfile()
    writeLines(c('"a"', '"b"', 'null', '"c"'), file)

    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(as.data.frame(ds),
                 structure(list(ds = c("a", "b", NA, "c")),
                           row.names = c(NA, -4),
                           class = c("corpus_frame", "data.frame")))

    expect_equal(as.data.frame(ds, row.names = c("w", "x", "y", "z")),
                 structure(list(ds = c("a", "b", NA, "c")),
                           row.names = c("w", "x", "y", "z"),
                           class = c("corpus_frame", "data.frame")))

    expect_equal(as.data.frame(ds, stringsAsFactors = TRUE),
                 structure(list(ds = factor(c("a", "b", NA, "c"))),
                           row.names = c(NA, -4),
                           class = c("corpus_frame", "data.frame")))
})


test_that("indexing with empty should succeed", {
    file <- tempfile()
    writeLines(c('"a"', '"b"', 'null', '"c"'), file)
    ds <- read_ndjson(file, simplify = FALSE)
    expect_equal(as.character(ds[]), as.character(ds))
    expect_equal(as.character(ds[integer()]), character())
})


test_that("invalid operations should fail", {
    file <- tempfile()
    writeLines(c('"a"', '"b"', 'null', '"c"'), file)
    ds <- read_ndjson(file, simplify = FALSE)

    expect_error(ds$hello, "$ operator is invalid for scalar JSON objects",
                 fixed = TRUE)
    expect_error(ds[["1"]], "invalid subscript: \"1\"", fixed = TRUE)
    expect_error(ds[1, 1], "incorrect number of dimensions")
})

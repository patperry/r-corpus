context("json_scalar")


test_that("reading integer works", {
    x <- c(48, -18, 42, 50, -4, 28, -18, -26, 11)
    file <- tempfile()
    writeLines(as.character(x), file)
    ds <- read_ndjson(file, simplify=FALSE)

    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as.integer(ds), x)
})


test_that("subscripting and subsetting integer works", {
    x <- c(-194, 273, -112, -76, 38, 0, 0, 99)
    file <- tempfile()
    writeLines(as.character(x), file)
    ds <- read_ndjson(file, simplify=FALSE)

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
    ds <- read_ndjson(file, simplify=FALSE)

    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as.numeric(ds), x)
})


test_that("subscripting and subsetting double works", {
    x <- c(-1.6571900, 0.1579793, -1.7205961, 0.1625917, 0.3143757, -1.1230602)
    file <- tempfile()
    writeLines(as.character(x), file)
    ds <- read_ndjson(file, simplify=FALSE)

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
    ds <- read_ndjson(file, simplify=FALSE)

    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as_text(ds), as_text(x))
    expect_equal(as.character(ds), x)
})


test_that("reading boolean works", {

    x <- c(TRUE, FALSE, NA, TRUE, TRUE, FALSE, NA)
    file <- tempfile()
    writeLines(ifelse(is.na(x), "null", ifelse(x, "true", "false")), file)
    ds <- read_ndjson(file, simplify=FALSE)

    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as.logical(ds), x)
})


test_that("reading empty works", {
    file <- tempfile()
    writeLines(character(), file)
    ds <- read_ndjson(file, simplify=FALSE)

    expect_equal(length(ds), 0)
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
})


test_that("reading double array words", {
    x <- list(3.14, c(1, 2, 3), c(), c(5.6, -3.0),
              c(2.18, 0.0028, 1e99, -2.1e12))
    file <- tempfile()
    writeLines(sapply(x, function(xi)
                      paste0("[", paste0(xi, collapse=", "), "]")), file)
    ds <- read_ndjson(file, simplify=FALSE)
    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as.list(ds), x)
})



test_that("reading integer array words", {
    x <- list(c(4L,-1L,2L), c(), c(), c(1L, 1L, 2L, 3L, 5L))
    file <- tempfile()
    writeLines(sapply(x, function(xi)
                      paste0("[", paste0(xi, collapse=", "), "]")), file)
    ds <- read_ndjson(file, simplify=FALSE)
    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as.list(ds), x)
})


test_that("reading character array works", {
    x <- list(c("hello", "world"), "how", c("are", "you", "?"))
    file <- tempfile()
    writeLines(sapply(x, function(xi)
                      paste0('["', paste0(xi, collapse='", "'), '"]')), file)
    ds <- read_ndjson(file, simplify=FALSE)
    expect_equal(length(ds), length(x))
    expect_equal(dim(ds), NULL)
    expect_equal(names(ds), NULL)
    expect_equal(as.list(ds), x)
})


test_that("reading record array works", {
    json <- c('[{"a": true, "b": false, "c": null}]',
              '[{"a": false, "c": true}]',
              '[]',
              '[{}]',
              '[{"a": true}, {"b": null}, {"c": true}]')
    x <- list(list(list(a = TRUE, b = FALSE, c = NULL)),
              list(list(a = FALSE, c = TRUE)),
              NULL,
              list(list()),
              list(list(a = TRUE), list(b = NULL), list(c = TRUE)))

    file <- tempfile()
    writeLines(json, file)
    ds <- read_ndjson(file, simplify=FALSE)
    expect_equal(as.list(ds), x)
})

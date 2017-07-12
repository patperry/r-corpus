context("json_serialize")


test_that("serializing json works", {
    x <- c("S", "P", "Q", "R")
    file <- tempfile()
    writeLines(paste0('"', x, '"'), file)
    ds <- read_ndjson(file, simplify=FALSE)

    file2 <- tempfile()
    saveRDS(ds, file2)
    ds2 <- readRDS(file2)

    expect_equal(as.character(ds), as.character(ds2))
})


test_that("serializing mmapped json works", {
    x <- c("S", "P", "Q", "R")
    file <- tempfile()
    writeLines(paste0('"', x, '"'), file)
    ds <- read_ndjson(file, mmap=TRUE, simplify=FALSE)

    file2 <- tempfile()
    saveRDS(ds, file2)
    ds2 <- readRDS(file2)

    expect_equal(as.character(ds), as.character(ds2))
})


test_that("serializing mmapped json should use relative, not absolute path", {
    wd <- getwd()
    on.exit(setwd(wd))

    x <- c("S", "P", "Q", "R")

    # create and change directory to dir/a
    dir <- tempfile()
    dir.create(dir)
    setwd(dir)
    dir.create("a")
    setwd("a")

    # save dir/a/data.json
    # save dir/a/obj.rds
    writeLines(paste0('{"x": "', x, '"}'), "data.json")
    ds <- read_ndjson("data.json", mmap=TRUE, simplify=FALSE)
    saveRDS(ds, "obj.rds")

    # move the files to
    # dir/data.json
    # dir/obj.rds
    file.rename(file.path(dir, "a", "data.json"), file.path(dir, "data.json"))
    file.rename(file.path(dir, "a", "obj.rds"), file.path(dir, "obj.rds"))

    # set the working directory to dir
    setwd(dir)
    unlink(file.path(dir, "a"), recursive=TRUE)

    # read obj.rds
    ds2 <- readRDS("obj.rds")
    expect_equal(as.character(ds2$x), x)
})


test_that("serializing json subset works", {
    x <- LETTERS
    file <- tempfile()
    writeLines(paste0('"', x, '"'), file)
    ds <- read_ndjson(file, simplify=FALSE)

    i <- seq(2, 26, 2)
    ds <- ds[i]

    file2 <- tempfile()
    saveRDS(ds, file2)
    ds2 <- readRDS(file2)

    expect_equal(as.character(ds), as.character(ds2))
})


test_that("serializing json field works", {
    x <- LETTERS
    y <- 3.14 * seq_along(LETTERS) - 10
    file <- tempfile()
    writeLines(paste0('{"x": "', x, '", "z": { "y": ', y, "} }"), file)
    ds <- read_ndjson(file, simplify=FALSE)

    ds <- ds$z

    file2 <- tempfile()
    saveRDS(ds, file2)
    ds2 <- readRDS(file2)

    expect_equal(as.numeric(ds$y), as.numeric(ds2$y))
})


test_that("serializing json nested fields works", {
    x <- 1:10
    file <- tempfile()
    writeLines(paste0('{"f1": {"f2": {"f3": {"x": ', x, '}}}}'), file)
    ds <- read_ndjson(file, simplify=FALSE)

    ds <- ds$f1$f2$f3

    file2 <- tempfile()
    saveRDS(ds, file2)
    ds2 <- readRDS(file2)

    expect_equal(as.integer(ds$x), as.numeric(ds2$x))
})


test_that("serializing json field subset works", {
    x <- LETTERS
    y <- 3.14 * seq_along(LETTERS) - 10
    file <- tempfile()
    writeLines(paste0('{"z": {"x": "', x, '"}, "y": ', y, "}"), file)
    ds <- read_ndjson(file, simplify = FALSE)

    i <- c(20, 2, 9, 4, 6, 2)
    ds <- ds[i, "z"]

    file2 <- tempfile()
    saveRDS(ds, file2)
    ds2 <- readRDS(file2)

    expect_equal(as.character(ds$x), as.character(ds2$x))
})

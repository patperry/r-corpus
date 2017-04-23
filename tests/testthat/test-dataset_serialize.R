context("dataset_serialize")


test_that("serializing dataset works", {
    x <- c("S", "P", "Q", "R")
    file <- tempfile()
    writeLines(paste0('"', x, '"'), file)
    ds <- read_json(file)

    file2 <- tempfile()
    saveRDS(ds, file2)
    ds2 <- readRDS(file2)
    
    expect_equal(as.character(ds), as.character(ds2))

    rm("ds", "ds2"); gc()
    file.remove(file)
    file.remove(file2)
})


test_that("serializing dataset subset works", {
    x <- LETTERS
    file <- tempfile()
    writeLines(paste0('"', x, '"'), file)
    ds <- read_json(file)

    i <- seq(2, 26, 2)
    ds <- ds[i]

    file2 <- tempfile()
    saveRDS(ds, file2)
    ds2 <- readRDS(file2)

    expect_equal(as.character(ds), as.character(ds2))

    rm("ds", "ds2"); gc()
    file.remove(file)
    file.remove(file2)
})


test_that("serializing dataset field works", {
    x <- LETTERS
    y <- 3.14 * seq_along(LETTERS) - 10
    file <- tempfile()
    writeLines(paste0('{"x": "', x, '", "z": { "y": ', y, "} }"), file)
    ds <- read_json(file)

    ds <- ds$z

    file2 <- tempfile()
    saveRDS(ds, file2)
    ds2 <- readRDS(file2)

    expect_equal(as.numeric(ds$y), as.numeric(ds2$y))

    rm("ds", "ds2"); gc()
    file.remove(file)
    file.remove(file2)
})


test_that("serializing dataset nested fields works", {
    x <- 1:10
    file <- tempfile()
    writeLines(paste0('{"f1": {"f2": {"f3": {"x": ', x, '}}}}'), file)
    ds <- read_json(file)

    ds <- ds$f1$f2$f3

    file2 <- tempfile()
    saveRDS(ds, file2)
    ds2 <- readRDS(file2)

    expect_equal(as.integer(ds$x), as.numeric(ds2$x))

    rm("ds", "ds2"); gc()
    file.remove(file)
    file.remove(file2)
})


test_that("serializing dataset field subset works", {
    x <- LETTERS
    y <- 3.14 * seq_along(LETTERS) - 10
    file <- tempfile()
    writeLines(paste0('{"z": {"x": "', x, '"}, "y": ', y, "}"), file)
    ds <- read_json(file)

    ds <- ds$z
    i <- c(20, 2, 9, 4, 6, 2)
    ds <- ds[i,]

    file2 <- tempfile()
    saveRDS(ds, file2)
    ds2 <- readRDS(file2)

    expect_equal(as.character(ds$x), as.character(ds2$x))

    rm("ds", "ds2"); gc()
    file.remove(file)
    file.remove(file2)
})

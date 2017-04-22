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

context("text_print")

test_that("'print.text' works without names", {
    x <- as_text(LETTERS)
    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]],
                 paste0(format(paste0("[", 1:26, "]"), justify = "right"),
                        " \"", LETTERS, "\""))
})


test_that("'print.text' works with names", {
    x <- as_text(LETTERS, names = paste0("foo", 1:26))
    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]],
                 paste0(format(names(x), justify = "left"),
                        " \"", LETTERS, "\""))
})

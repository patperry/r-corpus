context("text_print")

test_that("'print.text' works without names", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    x <- as_text(LETTERS)
    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]],
                 c(paste0(format(paste0("[", 1:20, "]"), justify = "right"),
                          " ", LETTERS[1:20]),
                   "...  (26 entries total)"))
})


test_that("'print.text' works with names", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    x <- as_text(LETTERS, names = paste0("foo", 1:26))
    expect_equal(strsplit(capture_output(print(x)), "\n")[[1]],
                 c(paste0(format(names(x)[1:20], justify = "left"),
                          " ", LETTERS[1:20]),
                   "...   (26 entries total)"))
})

context("text_print")

test_that("'print.text' works without names", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    x <- as_corpus_text(LETTERS)
    expected <- c(
' [1] "A" "B" "C" "D" "E" "F" "G" "H" "I"',
'[10] "J" "K" "L" "M" "N" "O" "P" "Q" "R"',
'[19] "S" "T"',
'...  (26 entries total)')

    expect_equal(strsplit(capture_output(print(x), width = 40), "\n")[[1]],
                 expected)
})


test_that("'print.text' works with names", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    x <- as_corpus_text(LETTERS, names = paste0("foo", 1:26))
    expected <- c(
'foo1  foo2  foo3  foo4  foo5  foo6  foo7  foo8 ',
'"A"   "B"   "C"   "D"   "E"   "F"   "G"   "H"  ',
'foo9  foo10 foo11 foo12 foo13 foo14 foo15 foo16',
'"I"   "J"   "K"   "L"   "M"   "N"   "O"   "P"  ',
'foo17 foo18 foo19 foo20',
'"Q"   "R"   "S"   "T"  ',
'... (26 entries total)')

    expect_equal(strsplit(capture_output(print(x), width = 50), "\n")[[1]],
                 expected)
})


test_that("'print.text' works for empty", {
    x <- as_corpus_text(character())
    expect_equal(capture_output(print(x)),
                 "text vector with 0 entries")
})


test_that("'print.text' works for NULL", {
    expect_equal(print.corpus_text(NULL), NULL)
})


test_that("'print.text' errors for invalid", {
    expect_error(print.corpus_text("hello"), "argument is not a valid text object")
})


test_that("'print.text' with negative rows prints entire object", {
    x <- as_corpus_text(LETTERS)
    expect_equal(capture_output(print(x, -1)),
                 capture_output(print(as.character(x))))
})


test_that("'print.text' errors for invalid inputs", {
    x <- as_corpus_text(LETTERS)
    expect_error(print(x, chars = -1), "'chars' must be non-negative")
    expect_error(print(x, chars = NA), "'chars' cannot be NA")
    expect_error(print(x, chars = c(1,1)), "'chars' must have length 1")
    expect_error(print(x, na.print = NA), "'na.print' cannot be NA")
    expect_error(print(x, print.gap = 1025), "'print.gap' must be less than or equal to 1024")
})


test_that("'format.text' works for empty", {
    expect_equal(format(as_corpus_text(character())), character())
})

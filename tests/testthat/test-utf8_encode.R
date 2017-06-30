context("utf8_encode")

switch_ctype <- function(mode = c("C", "Unicode"))
{
    mode <- match.arg(mode)

    if (mode == "Unicode") {
        sysname <- Sys.info()[["sysname"]]
        if (sysname == "windows") {
            ctype <- "English_United States.1252"
        } else if (sysname == "Darwin") {
            ctype <- "UTF-8"
        } else {
            ctype <- "en_US.utf8"
        }
    } else {
        ctype <- "C"
    }

    ctype0 <- Sys.getlocale("LC_CTYPE")
    Sys.setlocale("LC_CTYPE", ctype)
    if (Sys.getlocale("LC_CTYPE") != ctype) {
        skip(paste0("Cannot change locale to '", ctype, "'"))
    }

    ctype0
}


test_that("'utf8_encode' can encode an ASCII string", {
    expect_equal(utf8_encode("hello"), "hello")
})


test_that("'utf8_encode' can encode NULL or an NA string", {
    expect_equal(utf8_encode(NULL), NULL)
    expect_equal(utf8_encode(NA_character_), NA_character_)
})


test_that("'utf8_encode' preserves attributes", {
    x <- matrix(LETTERS, 2, 13)
    x[1,4] <- "\xa4"
    Encoding(x) <- "latin1"
    dimnames(x) <- list(c("foo", "bar"), as.character(1:13))
    class(x) <- "my_class"

    switch_ctype("Unicode")
    expect_equal(utf8_encode(x), enc2utf8(x))
})


test_that("'utf8_encode' can encode basic Unicode", {
    x <- "\u200b"
    Encoding(x) <- "UTF-8"

    switch_ctype("Unicode")
    expect_equal(utf8_encode(x), x)
})


test_that("'utf8_encode' can encode extended Unicode", {
    x <- "\U0001f60d"
    Encoding(x) <- "UTF-8"

    switch_ctype("Unicode")
    expect_equal(utf8_encode(x), x)
})

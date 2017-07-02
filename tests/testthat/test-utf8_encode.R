context("utf8_encode")

switch_ctype <- function(mode = c("C", "Unicode"))
{
    mode <- match.arg(mode)

    if (mode == "Unicode") {
        sysname <- Sys.info()[["sysname"]]
        if (sysname == "Windows") {
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
    suppressWarnings({
        Sys.setlocale("LC_CTYPE", ctype)
    })
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

    ctype <- switch_ctype("Unicode")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_encode(x), enc2utf8(x))
})


test_that("'utf8_encode' can encode basic Unicode", {
    x <- "\u200b"
    Encoding(x) <- "UTF-8"

    ctype <- switch_ctype("Unicode")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_encode(x), x)
})


test_that("'utf8_encode' can encode extended Unicode", {
    x <- intToUtf8(0x0001f60d)
    Encoding(x) <- "UTF-8"

    ctype <- switch_ctype("Unicode")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_encode(x), x)
})


test_that("'utf8_encode' can handle ASCII escapes", {
    x <- "\x01\a\b\f\n\r\t\v\x7f"
    expect_equal(utf8_encode(x), "\\x01\\a\\b\\f\\n\\r\\t\\v\\x7f")
})

test_that("'utf8_encode' can handle invalid UTF-8", {
    x <- "\xfe"
    Encoding(x) <- "bytes"
    expect_equal(utf8_encode(x), "\\xfe")
})

test_that("'utf8_encode' can handle bytes", {
    x <- "\x01\a\b\f\n\r\t\v\x7f\x80\xff"
    Encoding(x) <- "bytes"
    expect_equal(utf8_encode(x),
                 "\\x01\\a\\b\\f\\n\\r\\t\\v\\x7f\\x80\\xff")
})

test_that("'utf8_encode' can handle latin-1", {
    x <- "her \xa320"
    Encoding(x) <- "latin1"

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_encode(x), "her \\u00a320")

    switch_ctype("Unicode")
    expect_equal(utf8_encode(x), "her \u00a320")
})

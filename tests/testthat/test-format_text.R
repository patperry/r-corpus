context("format.text")


test_that("'format' can handle short text", {
    raw <- c(NA, "", "a", "foo", "short text")
    text <- as_text(raw)

    expect_equal(format(text, justify = "none"),
                 format(raw, justify = "none"))

    expect_equal(format(text, justify = "left"),
                 format(raw, justify = "left"))

    expect_equal(format(text, justify = "centre"),
                 format(raw, justify = "centre"))

    expect_equal(format(text, justify = "right"),
                 format(raw, justify = "right"))
})


test_that("'format' can handle long text in UTF-8 locale", {
    raw    <- c(NA, "", "a", "ab", "foo", "food",     "short text",
                "\u6027", "\u6027\u6027", "\u6027?", "\U0001f642")
    short  <- c(NA, "", "a", "ab", "fo\u2026", "fo\u2026", "sh\u2026",
                "\u6027", "\u6027\u2026", "\u6027\u2026", "\u2026")
    rshort <- c(NA, "", "a", "ab", "\u2026oo", "\u2026od", "\u2026xt",
                "\u6027", "\u2026\u6027", "\u2026?", "\u2026")
    text <- as_text(raw)

    ctype <- Sys.getlocale("LC_CTYPE")
    Sys.setlocale("LC_CTYPE", "UTF-8")

    expect_equal(format(text, chars = 2, justify = "none"),
                 format(short, justify = "none"))

    expect_equal(format(text, chars = 2, justify = "left"),
                 format(short, justify = "left"))

    expect_equal(format(text, chars = 2, justify = "centre"),
                 format(short, justify = "centre"))

    expect_equal(format(text, chars = 2, justify = "right"),
                 format(rshort, justify = "right"))

    Sys.setlocale("LC_CTYPE", ctype)
})


test_that("'format' can handle long text in UTF-8 locale, part 2", {
    raw    <- c(NA, "", "a", "ab", "foo", "food",     "short text",
                "\u6027", "\u6027\u6027", "\u6027?", "\U0001f642")
    short  <- c(NA, "", "a", "a\u2026", "f\u2026", "f\u2026", "s\u2026",
                "\u2026", "\u2026", "\u2026", "\u2026")
    rshort <- c(NA, "", "a", "\u2026b", "\u2026o", "\u2026d", "\u2026t",
                "\u2026", "\u2026", "\u2026?", "\u2026")
    text <- as_text(raw)

    ctype <- Sys.getlocale("LC_CTYPE")
    Sys.setlocale("LC_CTYPE", "UTF-8")

    expect_equal(format(text, chars = 1, justify = "none"),
                 format(short, justify = "none"))

    expect_equal(format(text, chars = 1, justify = "left"),
                 format(short, justify = "left"))

    expect_equal(format(text, chars = 1, justify = "centre"),
                 format(short, justify = "centre"))

    expect_equal(format(text, chars = 1, justify = "right"),
                 format(rshort, justify = "right"))

    Sys.setlocale("LC_CTYPE", ctype)
})


test_that("'format' can handle long text in C locale", {
    raw    <- c("\u6027", "?\u6027", "\n\u6027", "?\n\u6027", "\u0001\u6027",
                "\u6027?", "\u6027\n", "\u6027?\n", "\u6027\u0001",
                "\U0001f642")
    short <- c("<U+6027>", "?<U+6027>", "\n<U+6027>", "?\n...",
                  "\001...", "<U+6027>?", "<U+6027>\n", "<U+6027>?...",
                  "<U+6027>...", "...")
    rshort <- c("<U+6027>", "?<U+6027>", "\n<U+6027>", "...\n<U+6027>",
                 "...<U+6027>", "<U+6027>?", "<U+6027>\n", "...?\n",
                  "...\001", "...")
    text <- as_text(raw)

    ctype <- Sys.getlocale("LC_CTYPE")
    Sys.setlocale("LC_CTYPE", "C")

    expect_equal(format(text, chars = 10, justify = "none"),
                 format(short, justify = "none"))

    expect_equal(format(text, chars = 10, justify = "left"),
                 format(short, justify = "left"))

    expect_equal(format(text, chars = 10, justify = "centre"),
                 format(short, justify = "centre"))

    expect_equal(format(text, chars = 10, justify = "right"),
                 format(rshort, justify = "right"))

    Sys.setlocale("LC_CTYPE", ctype)
})

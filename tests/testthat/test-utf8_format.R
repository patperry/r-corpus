context("utf8_format")


test_that("'format' can handle short text", {
    raw <- c(NA, "", "a", "foo", "short text")
    text <- as_text(raw)

    expect_equal(format(text, justify = "none"), format(raw, justify = "none"))
    expect_equal(utf8_format(raw, justify = "none"), format(raw, justify = "none"))

    expect_equal(format(text, justify = "left"), format(raw, justify = "left"))
    expect_equal(utf8_format(raw, justify = "left"), format(raw, justify = "left"))

    expect_equal(format(text, justify = "centre"), format(raw, justify = "centre"))
    expect_equal(utf8_format(raw, justify = "centre"), format(raw, justify = "centre"))

    expect_equal(format(text, justify = "right"), format(raw, justify = "right"))
    expect_equal(utf8_format(raw, justify = "right"), format(raw, justify = "right"))
})


test_that("'format' can handle long text in Unicode locale", {
    raw    <- c(NA, "", "a", "ab", "foo", "food",     "short text",
                "\u6027", "\u6027\u6027", "\u6027?")
    Encoding(raw) <- "UTF-8"

    short  <- c(NA, "", "a", "ab", "fo\u2026", "fo\u2026", "sh\u2026",
                "\u6027", "\u6027\u2026", "\u6027\u2026")
    Encoding(short) <- "UTF-8"

    rshort <- c(NA, "", "a", "ab", "\u2026oo", "\u2026od", "\u2026xt",
                "\u6027", "\u2026\u6027", "\u2026?")
    Encoding(rshort) <- "UTF-8"

    text <- as_text(raw)

    ctype <- switch_ctype("Unicode")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(format(text, chars = 2, justify = "none"), format(short, justify = "none"))
    expect_equal(utf8_format(raw, chars = 2, justify = "none"), format(short, justify = "none"))

    expect_equal(format(text, chars = 2, justify = "left"), format(short, justify = "left"))
    expect_equal(utf8_format(raw, chars = 2, justify = "left"), format(short, justify = "left"))

    expect_equal(format(text, chars = 2, justify = "centre"), format(short, justify = "centre"))
    expect_equal(utf8_format(raw, chars = 2, justify = "centre"), format(short, justify = "centre"))

    expect_equal(format(text, chars = 2, justify = "right"), format(rshort, justify = "right"))
    expect_equal(utf8_format(raw, chars = 2, justify = "right"), format(rshort, justify = "right"))
})


test_that("'format' can handle long text in UTF-8 locale, part 2", {
    raw    <- c(NA, "", "a", "\n", "ab", "foo", "food",     "short text",
                "\u6027", "\u6027\u6027", "\u6027?")
    short  <- c(NA, "", "a", "\u2026", "a\u2026", "f\u2026", "f\u2026",
                "s\u2026", "\u2026", "\u2026", "\u2026")
    rshort <- c(NA, "", "a", "\u2026", "\u2026b", "\u2026o", "\u2026d",
                "\u2026t", "\u2026", "\u2026", "\u2026?")
    text <- as_text(raw)

    ctype <- switch_ctype("Unicode")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(format(text, chars = 1, justify = "none"), format(short, justify = "none"))
    expect_equal(utf8_format(raw, chars = 1, justify = "none"), format(short, justify = "none"))

    expect_equal(format(text, chars = 1, justify = "left"), format(short, justify = "left"))
    expect_equal(utf8_format(raw, chars = 1, justify = "left"), format(short, justify = "left"))

    expect_equal(format(text, chars = 1, justify = "centre"), format(short, justify = "centre"))
    expect_equal(utf8_format(raw, chars = 1, justify = "centre"), format(short, justify = "centre"))

    expect_equal(format(text, chars = 1, justify = "right"), format(rshort, justify = "right"))
    expect_equal(utf8_format(raw, chars = 1, justify = "right"), format(rshort, justify = "right"))
})


test_that("'format' can handle long text in C locale", {
    #            6         7          8           9            10
    raw    <- c("\u6027", "?\u6027", "\n\u6027", "?\n\u6027", "\u0001\u6027",
                          "\u6027?", "\u6027\n", "\u6027?\n", "\u6027\u0001")
    short  <- c("\\u6027", "?\\u6027", "\\n\\u6027", "?\\n...",     "\\u0001...",
                          "\\u6027?", "\\u6027\\n", "\\u6027?...", "\\u6027...")
    rshort <- c("\\u6027", "?\\u6027", "\\n\\u6027", "...\\n\\u6027",
                 "...\\u6027", "\\u6027?", "\\u6027\\n", "...?\\n",
                  "...\\u0001")
    text <- as_text(raw)

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(utf8_encode(format(text, chars = 8, justify = "none")), format(short, justify = "none"))

    left <- utf8_encode(format(text, chars = 8, justify = "left"))
    expect_equal(sub("\\s+$", "", left), short)
    expect_equal(nchar(left), rep(10, length(text)))

    centre <- utf8_encode(format(text, chars = 8, justify = "centre"))
    expect_equal(sub("^\\s+", "", sub("\\s+$", "", centre)), short)
    expect_equal(nchar(centre), rep(10, length(text)))

    right <- utf8_encode(format(text, chars = 8, justify = "right"))
    expect_equal(sub("^\\s+", "", right), rshort)
    expect_equal(nchar(right), rep(11, length(text)))

    expect_equal(format(text, chars = 8, justify = "none"), utf8_format(raw, chars = 8, justify = "none"))
    expect_equal(format(text, chars = 8, justify = "left"), utf8_format(raw, chars = 8, justify = "left"))
    expect_equal(format(text, chars = 8, justify = "centre"), utf8_format(raw, chars = 8, justify = "centre"))
    expect_equal(format(text, chars = 8, justify = "right"), utf8_format(raw, chars = 8, justify = "right"))
})


test_that("'format' can handle high code points in C locale", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    raw <- c(intToUtf8(0x00010000), intToUtf8(0x0010ffff))
    text <- as_text(raw)
    fmt <- raw

    expect_equal(format(text, justify = "left"), fmt)
    expect_equal(utf8_format(raw, justify = "left"), fmt)
    expect_equal(format(text, justify = "right"), fmt)
    expect_equal(utf8_format(raw, justify = "right"), fmt)
})


test_that("'format' can handle high code points in Unicode locale", {
    ctype <- switch_ctype("Unicode")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    raw   <- c(intToUtf8(0x00010000), intToUtf8(0x010ffff))
    left  <- c(paste0(intToUtf8(0x00010000), "         "), intToUtf8(0x010ffff))
    right <- c(paste0("         ", intToUtf8(0x00010000)), intToUtf8(0x010ffff))

    text <- as_text(raw)
    expect_equal(format(text, justify = "left"), left)
    expect_equal(utf8_format(raw, justify = "left"), left)
    expect_equal(format(text, justify = "right"), right)
    expect_equal(utf8_format(raw, justify = "right"), right)
})


test_that("'format' can handle ignorable code points", {
    raw <- "\u200B"
    text <- as_text(raw)

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(format(text, justify = "left"), raw)
    expect_equal(format(text, justify = "centre"), raw)
    expect_equal(format(text, justify = "right"), raw)

    expect_equal(utf8_format(raw, justify = "left"), raw)
    expect_equal(utf8_format(raw, justify = "centre"), raw)
    expect_equal(utf8_format(raw, justify = "right"), raw)

    switch_ctype("Unicode")

    expect_equal(format(text, justify = "left"), raw)
    expect_equal(format(text, justify = "centre"), raw)
    expect_equal(format(text, justify = "right"), raw)

    expect_equal(utf8_format(raw, justify = "left"), raw)
    expect_equal(utf8_format(raw, justify = "centre"), raw)
    expect_equal(utf8_format(raw, justify = "right"), raw)
})


test_that("'format' can handle marks", {
    raw <- "e\u0300"
    text <- as_text(raw)

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(format(text, chars = 1, justify = "left"), "e...")
    expect_equal(utf8_format(raw, chars = 1, justify = "left"), "e...")

    expect_equal(format(text, chars = 1, justify = "centre"), "e...")
    expect_equal(utf8_format(raw, chars = 1, justify = "centre"), "e...")

    expect_equal(format(text, chars = 1, justify = "right"), "...")
    expect_equal(utf8_format(raw, chars = 1, justify = "right"), "...")

    ctype <- switch_ctype("Unicode")

    expect_equal(format(text, chars = 1, justify = "left"), raw)
    expect_equal(format(text, chars = 1, justify = "centre"), raw)
    expect_equal(format(text, chars = 1, justify = "right"), raw)
    expect_equal(utf8_format(raw, chars = 1, justify = "left"), raw)
    expect_equal(utf8_format(raw, chars = 1, justify = "centre"), raw)
    expect_equal(utf8_format(raw, chars = 1, justify = "right"), raw)
})


test_that("'format' can handle UTF-8 'Other' codes", {
    raw <- "\u2072" # unassigned
    text <- as_text(raw)

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(format(text, justify = "left"), raw)
    expect_equal(format(text, justify = "centre"), raw)
    expect_equal(format(text, justify = "right"), raw)
    expect_equal(utf8_format(raw, justify = "left"), raw)
    expect_equal(utf8_format(raw, justify = "centre"), raw)
    expect_equal(utf8_format(raw, justify = "right"), raw)

    switch_ctype("Unicode")

    expect_equal(format(text, justify = "left"), raw)
    expect_equal(format(text, justify = "centre"), raw)
    expect_equal(format(text, justify = "right"), raw)
    expect_equal(utf8_format(raw, justify = "left"), raw)
    expect_equal(utf8_format(raw, justify = "centre"), raw)
    expect_equal(utf8_format(raw, justify = "right"), raw)
})


test_that("'format' can handle negative or zero, or NA chars", {
    text <- as_text("foo")

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))
    
    expect_equal(format(text, chars = -1, justify = "left"), "...")
    expect_equal(format(text, chars = -1, justify = "centre"), "...")
    expect_equal(format(text, chars = -1, justify = "right"), "...")

    expect_equal(format(text, chars = 0, justify = "left"), "...")
    expect_equal(format(text, chars = 0, justify = "centre"), "...")
    expect_equal(format(text, chars = 0, justify = "right"), "...")

    expect_equal(format(text, chars = NA, justify = "left"), "foo")
    expect_equal(format(text, chars = NA, justify = "centre"), "foo")
    expect_equal(format(text, chars = NA, justify = "right"), "foo")
})


test_that("'format' can skip NA", {
    text <- as_text(NA)
    expect_equal(format(text, na.encode = FALSE), NA_character_)
    expect_equal(utf8_format(NA_character_, na.encode = FALSE), NA_character_)
})


test_that("'format' can set minimum width", {
    raw <- c("a", "ab", "abc")
    text <- as_text(raw)

    expect_equal(format(text, justify = "none", width = 5),
                 format(raw, justify = "none", width = 5))
    expect_equal(format(text, justify = "left", width = 5),
                 format(raw, justify = "left", width = 5))
    expect_equal(format(text, justify = "centre", width = 5),
                 format(raw, justify = "centre", width = 5))
    expect_equal(format(text, justify = "right", width = 5),
                 format(raw, justify = "right", width = 5))
})


test_that("'format' error for invalid justify", {
    text <- as_text("")
    expect_error(format(text, justify = "wild"),
                 paste("'justify' must be one of",
                       paste(dQuote(c("left", "right", "centre", "none")),
                             collapse = ", ")),
                 fixed = TRUE)
})


test_that("'format' error for invalid logicals", {
    text <- as_text("")
    expect_error(format(text, trim = NA), "'trim' must be TRUE or FALSE",
                 fixed = TRUE)
    expect_error(format(text, na.encode = NA),
                 "'na.encode' must be TRUE or FALSE", fixed = TRUE)
})


test_that("'format' error for invalid integers", {
    text <- as_text("")
    expect_error(format(text, chars = "3"),
                 "'chars' must be an integer scalar",
                 fixed = TRUE)
    expect_error(format(text, width = "3"),
                 "'width' must be an integer scalar",
                 fixed = TRUE)
})


test_that("'utf8_format' can handle invalid UTF-8", {
    invalid <- "\xff\xfe"
    Encoding(invalid) <- "UTF-8"
    x <- c("a", invalid)

    expect_equal(utf8_format(x, justify = "none"), x)
    expect_equal(utf8_format(x, justify = "left"), c("a       ", invalid))
    expect_equal(utf8_format(x, justify = "centre"), c("   a    ", invalid))
    expect_equal(utf8_format(x, justify = "right"), c("       a", invalid))
})


test_that("'utf8_format' can handle latin1 text", {
    x <- "fa\xE7ile"
    Encoding(x) <- "latin1"
    y <- iconv(x, "latin1", "UTF-8")
    expect_equal(utf8_format(x), y)
})

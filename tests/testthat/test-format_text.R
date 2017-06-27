context("format.text")


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


test_that("'format' can handle long text in Unicode locale", {
    raw    <- c(NA, "", "a", "ab", "foo", "food",     "short text",
                "\u6027", "\u6027\u6027", "\u6027?", "\U0001f642")
    short  <- c(NA, "", "a", "ab", "fo\u2026", "fo\u2026", "sh\u2026",
                "\u6027", "\u6027\u2026", "\u6027\u2026", "\u2026")
    rshort <- c(NA, "", "a", "ab", "\u2026oo", "\u2026od", "\u2026xt",
                "\u6027", "\u2026\u6027", "\u2026?", "\u2026")
    text <- as_text(raw)

    ctype <- switch_ctype("Unicode")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(format(text, chars = 2, justify = "none"),
                 format(short, justify = "none"))

    expect_equal(format(text, chars = 2, justify = "left"),
                 format(short, justify = "left"))

    expect_equal(format(text, chars = 2, justify = "centre"),
                 format(short, justify = "centre"))

    expect_equal(format(text, chars = 2, justify = "right"),
                 format(rshort, justify = "right"))
})


test_that("'format' can handle long text in UTF-8 locale, part 2", {
    raw    <- c(NA, "", "a", "\n", "ab", "foo", "food",     "short text",
                "\u6027", "\u6027\u6027", "\u6027?", "\U0001f642")
    short  <- c(NA, "", "a", "\u2026", "a\u2026", "f\u2026", "f\u2026",
                "s\u2026", "\u2026", "\u2026", "\u2026", "\u2026")
    rshort <- c(NA, "", "a", "\u2026", "\u2026b", "\u2026o", "\u2026d",
                "\u2026t", "\u2026", "\u2026", "\u2026?", "\u2026")
    text <- as_text(raw)

    ctype <- switch_ctype("Unicode")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(format(text, chars = 1, justify = "none"),
                 format(short, justify = "none"))

    expect_equal(format(text, chars = 1, justify = "left"),
                 format(short, justify = "left"))

    expect_equal(format(text, chars = 1, justify = "centre"),
                 format(short, justify = "centre"))

    expect_equal(format(text, chars = 1, justify = "right"),
                 format(rshort, justify = "right"))
})


test_that("'format' can handle long text in C locale", {
    # R has a Unicode bug on Windows
    # https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=17299
    skip_on_os("windows")

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

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    expect_equal(format(text, chars = 10, justify = "none"),
                 format(short, justify = "none"))

    expect_equal(format(text, chars = 10, justify = "left"),
                 format(short, justify = "left"))

    expect_equal(format(text, chars = 10, justify = "centre"),
                 format(short, justify = "centre"))

    expect_equal(format(text, chars = 10, justify = "right"),
                 format(rshort, justify = "right"))
})


test_that("'format' can handle high code points in C locale", {
    skip_on_os("windows")

    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    text <- "\U00010000"
    expect_equal(format(text, justify="left"), "<U+00010000>")
    expect_equal(format(text, justify="right"), "<U+00010000>")
})


test_that("'format' can handle high code points in Unicode locale", {
    skip_on_os("windows")

    ctype <- switch_ctype("Unicode")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    text <- "\U00010000"
    expect_equal(format(text, justify="left"), text)
    expect_equal(format(text, justify="right"), text)
})

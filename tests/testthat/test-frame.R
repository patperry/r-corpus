
context("frame")

test_that("'print.corpus_frame' can print all rows", {
    d <- data.frame(x = 1:50)

    expect_equal(capture_output(print.corpus_frame(d, -1)),
                 capture_output(print.corpus_frame(d, .Machine$integer.max)))

    expect_equal(capture_output(print.corpus_frame(d, NULL)),
                 capture_output(print.corpus_frame(d, .Machine$integer.max)))

    expect_error(print.corpus_frame(d, NA), "'rows' cannot be NA")
})


test_that("'print.corpus_frame' produces the same results on ASCII", {
    d <- data.frame(x = 1:10, f = gl(2,5), ch = I(letters[1:10]))
    dr <- d
    dr$ch <- paste0(d$ch, " ")

    dq <- d
    dq$f <- paste0('"', d$f, '"')
    dq$ch <- paste0('"', d$ch, '"')
    names(dq) <- c("x", "f  ", "ch ")

    expect_equal(capture_output(print.corpus_frame(d)),
                 capture_output(print(dr)))
    expect_equal(
        capture_output(print.corpus_frame(d, quote = TRUE,
                                          row.names = FALSE)),
        capture_output(print(dq, row.names = FALSE)))
})


test_that("'print.corpus_frame' handles row names", {
    d <- data.frame(x = 1:10, f = gl(2,5), ch = I(letters[1:10]))
    row.names(d) <- LETTERS[1:10]

    dr <- d
    dr$ch <- paste0(d$ch, " ")

    dq <- d
    dq$f <- paste0('"', d$f, '"')
    dq$ch <- paste0('"', d$ch, '"')
    names(dq) <- c("x", "f  ", "ch ")

    expect_equal(capture_output(print.corpus_frame(d)),
                 capture_output(print(dr)))

    expect_equal(
        capture_output(print.corpus_frame(d, quote = TRUE,
                                          row.names = FALSE)),
        capture_output(print(dq, row.names = FALSE)))
})


test_that("'print.corpus_frame' wraps correctly", {
    w <- getOption("width")
    half <- floor(w / 2)
    d <- data.frame(x = c("X", paste(rep("x", 2 * w), collapse="")),
                    y = c("Y", paste(rep("y", half + 1), collapse="")),
                    z = c("Z", paste(rep("z", half + 1), collapse="")),
                    a = 1:2,
                    b = 3:4,
                    c = 5:6)

    expect_equal(capture_output(print.corpus_frame(d, chars = 1000)),
                 capture_output(print(d, right = FALSE)))

    d2 <- data.frame(x = paste(rep("x", w - 2), collapse=""), y = "y", z = "z")
    expect_equal(capture_output(print.corpus_frame(d2, chars = 1000)),
                 capture_output(print(d2, right = FALSE)))

    expect_equal(capture_output(print.corpus_frame(d2[,c(2,1,3)],
                                                   chars = 1000)),
                 capture_output(print(d2[,c(2,1,3)], right = FALSE)))

    expect_equal(capture_output(print.corpus_frame(d2[,c(2,3,1)],
                                                   chars = 1000)),
                 capture_output(print(d2[,c(2,3,1)], right = FALSE)))

    d3 <- data.frame(x = "X", y = "Y", z = "Z",
                     row.names = paste(rep("x", w), collapse=""))
    expect_equal(capture_output(print.corpus_frame(d3)),
                 capture_output(print(d3, right = FALSE)))

    d4 <- data.frame(x = "X", y = "Y", z = "Z",
                     row.names = paste(rep("x", w - 1), collapse=""))
    expect_equal(capture_output(print.corpus_frame(d4)),
                 capture_output(print(d4, right = FALSE)))
    
    d5 <- data.frame(x = "X", y = "Y", z = "Z",
                     row.names = paste(rep("x", w + 1), collapse=""))
    expect_equal(capture_output(print.corpus_frame(d5)),
                 capture_output(print(d5, right = FALSE)))
})


test_that("'print.corpus_frame' handles NA in column names", {
    d <- data.frame(x = 1:10, f = gl(2,5), ch = I(letters[1:10]))
    dr <- d
    dr$f <- paste0(d$f, " ")
    dr$ch <- paste0(d$ch, " ")

    names(d) <- c("x", NA, "ch")
    names(dr) <- c("x", NA, "ch")

    expect_equal(capture_output(print.corpus_frame(d)),
                 capture_output(print(dr)))
})


test_that("'print.corpus_frame' handles NA elements", {
    d <- data.frame(x = NA, ch = I(NA_character_),
                    f = as.factor(NA_character_))
    dr <- d
    names(dr) <- c("x", "ch  ", "f   ")

    dq <- d
    names(dq) <- c("x", "ch", "f ")

    dfoo <- d
    names(dfoo) <- c("x", "ch ", "f  ")

    expect_equal(capture_output(print.corpus_frame(d)),
                 capture_output(print(dr)))

    expect_equal(capture_output(print.corpus_frame(d, quote = TRUE)),
                 capture_output(print(dq, na.print = "NA")))

    expect_equal(capture_output(print.corpus_frame(d, na.print = "foo")),
                 capture_output(print(dfoo, na.print = "foo")))

    expect_equal(capture_output(print.corpus_frame(d, na.print = "foo",
                                                   quote = TRUE)),
                 capture_output(print(dfoo, na.print = "foo")))
})


test_that("'print.corpus_frame' handles NA row or column names", {
    d1 <- structure(list(x=1), row.names=NA_character_, class = "data.frame")
    expect_equal(capture_output(print.corpus_frame(d1)), "   x\nNA 1")

    x <- list(1)
    names(x) <- NA
    d2 <- structure(x, row.names="foo", class = "data.frame")
    expect_equal(capture_output(print.corpus_frame(d2)), "    NA\nfoo  1")
})


test_that("'print.corpus_frame' handles empty data frames", {
    # no row or column names
    d1 <- data.frame()
    expect_equal(capture_output(print.corpus_frame(d1)),
                 "data frame with 0 columns and 0 rows")

    # no row names
    d2 <- data.frame(a = integer(), b = integer(), "\n" = logical(),
                     check.names = FALSE)
    expect_equal(capture_output(print.corpus_frame(d2)), "a b \\n\n(0 rows)")

    # columns but no column names
    d3 <- structure(list(integer(), integer()),
                    class = "data.frame", row.names = c(NA, 0))
    expect_equal(capture_output(print.corpus_frame(d3)),
                 "data frame with 2 columns and 0 rows")
})

chartype_frame <- function()
{
    chars <- character()
    desc <- character()

    chars[1] <- "\u0001\u001f"; desc[1] <- "C0 control code"
    chars[2] <- "\a\b\f\n\r\t"; desc[2] <- "Named control code"
    chars[3] <- "abcdefuvwxyz"; desc[3] <- "ASCII"
    chars[4] <- "\u0080\u009f"; desc[4] <- "C1 control code"

    chars[5] <- paste0("\u00a0\u00a1\u00a2\u00a3\u00a4\u00a5",
                       "\u00fa\u00fb\u00fc\u00fd\u00fe\u00ff")
    desc[5] <- "Latin-1"

    chars[6] <- paste0("\u0100\u0101\u0102\u0103\u0104\u0105",
                       "\u0106\u0107\u0108\u0109\u010a\u010b")
    desc[6] <- "Unicode"

    chars[7] <- "\uff01\uff02\uff03\uff04\uff05\uff06"
    desc[7] <- "Unicode wide"

    chars[8] <- "\ue00\u2029"
    desc[8] <- "Unicode control"

    chars[9] <- paste0("x\u00adx\u200bx\u200cx\u200dx\u200ex\u200f",
                       "x\u034fx\ufeffx", intToUtf8(0xE0001), "x",
                       intToUtf8(0xE0020), "x", intToUtf8(0xE01EF), "x")
    desc[9] <- "Unicode ignorable"

    chars[10] <- paste0("a\u0300a\u0301a\u0302a\u0303a\u0304a\u0305",
                        "a\u0306a\u0307a\u0308a\u0309a\u030aa\u030b")
    desc[10] <- "Unicode mark"

    chars[11] <- paste0(intToUtf8(0x1F600), intToUtf8(0x1F601),
                        intToUtf8(0x1F602), intToUtf8(0x1F603),
                        intToUtf8(0x1F604), intToUtf8(0x1F483))
    desc[11] <- "Emoji"

    chars[12] <- paste0("x", intToUtf8(0x10ffff), "x")
    desc[12] <- "Unassigned"

    chars[13] <- "\xfd\xfe\xff"
    desc[13] <- "Invalid"

    Encoding(chars) <- "UTF-8"

    x <- data.frame(chars, desc, stringsAsFactors = FALSE)
}


test_that("'print.corpus_frame' handles Unicode correctly", {
    # R can't print all UTF-8 on windows:
    # https://stat.ethz.ch/pipermail/r-devel/2017-June/074556.html
    skip_on_os("windows")
    ctype <- switch_ctype("Unicode")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    x <- chartype_frame()
    actual <- strsplit(capture_output(print.corpus_frame(x, right = FALSE)),
                       "\n")[[1]]
    Encoding(actual) <- "UTF-8"

    expected <- c(
        "   chars        desc              ",
        "1  \\u0001\\u001f C0 control code   ",
        "2  \\a\\b\\f\\n\\r\\t Named control code",
        "3  abcdefuvwxyz ASCII             ",
        "4  \\u0080\\u009f C1 control code   ",
        paste0("5  ", x$chars[5], " Latin-1           "),
        paste0("6  ", x$chars[6], " Unicode           "),
        "7  \uff01\uff02\uff03\uff04\uff05\uff06 Unicode wide      ",
        "8  \\u0e00\\u2029 Unicode control   ",
        "9  xxxxxxxxxxxx Unicode ignorable ",
        paste0("10 ", x$chars[10], " Unicode mark      "),
        paste0("11 ", paste(intToUtf8(0x1F600), intToUtf8(0x1F601),
                            intToUtf8(0x1F602), intToUtf8(0x1F603),
                            intToUtf8(0x1F604), intToUtf8(0x1F483), "",
                            sep = "\u200b"), " Emoji             "),
        "12 x\\U0010ffffx Unassigned        ",
        "13 \\xfd\\xfe\\xff Invalid           ")
    Encoding(expected) <- "UTF-8"

    expect_equal(actual, expected)
})


test_that("'print.corpus_frame' works in C locale", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype))

    x <- chartype_frame()
    actual <- strsplit(capture_output(print.corpus_frame(x, right = FALSE,
                                                         chars = 1000)),
                       "\n")[[1]]

    expected <- c(
    "   chars                                                                                     ",
    "1  \\u0001\\u001f                                                                              ",
    "2  \\a\\b\\f\\n\\r\\t                                                                              ",
    "3  abcdefuvwxyz                                                                              ",
    "4  \\u0080\\u009f                                                                              ",
    "5  \\u00a0\\u00a1\\u00a2\\u00a3\\u00a4\\u00a5\\u00fa\\u00fb\\u00fc\\u00fd\\u00fe\\u00ff                  ",
    "6  \\u0100\\u0101\\u0102\\u0103\\u0104\\u0105\\u0106\\u0107\\u0108\\u0109\\u010a\\u010b                  ",
    "7  \\uff01\\uff02\\uff03\\uff04\\uff05\\uff06                                                      ",
    "8  \\u0e00\\u2029                                                                              ",
    "9  x\\u00adx\\u200bx\\u200cx\\u200dx\\u200ex\\u200fx\\u034fx\\ufeffx\\U000e0001x\\U000e0020x\\U000e01efx",
    "10 a\\u0300a\\u0301a\\u0302a\\u0303a\\u0304a\\u0305a\\u0306a\\u0307a\\u0308a\\u0309a\\u030aa\\u030b      ",
    "11 \\U0001f600\\U0001f601\\U0001f602\\U0001f603\\U0001f604\\U0001f483                              ",
    "12 x\\U0010ffffx                                                                              ",
    "13 \\xfd\\xfe\\xff                                                                              ",
    "   desc              ",
    "1  C0 control code   ",
    "2  Named control code",
    "3  ASCII             ",
    "4  C1 control code   ",
    "5  Latin-1           ",
    "6  Unicode           ",
    "7  Unicode wide      ",
    "8  Unicode control   ",
    "9  Unicode ignorable ",
    "10 Unicode mark      ",
    "11 Emoji             ",
    "12 Unassigned        ",
    "13 Invalid           ")

    expect_equal(actual, expected)
})


test_that("'print.corpus_frame' can right justify", {
    d <- data.frame(ch = c("a", "ab", "abc"))

    expect_equal(capture_output(print.corpus_frame(d, right = TRUE)),
                 capture_output(print(d, right = TRUE)))
})


test_that("'print.corpus_frame' does not need a gap at the end", {
    w <- getOption("width")
    d <- data.frame(x = paste0(rep("x", 10), collapse=""),
                    y = paste0(rep("y", w - 10 - 3), collapse=""))
    expect_equal(length(strsplit(capture_output(print.corpus_frame(d)),
                                 "\n")[[1]]), 2)
})


test_that("'print.corpus_frame' can wrap 4 columns", {
    ctype <- switch_ctype("C")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)

    oldwidth <- options()$width
    options(width = 80)
    on.exit(options(width = oldwidth), add = TRUE)

    x <- corpus_frame(
        title = "The Declaration of Independence of The United States of America",
        author = "Founding Fathers",
        language = "English",
        text = "The Declaration of Independence of The United States of America\n\n\nWhen in the course of human events")

    lines <- c(
'  title                                                          ',
'1 The Declaration of Independence of The United States of America',
'  author           language text                                                ',
'1 Founding Fathers English  The Declaration of Independence of The United Sta...')

    expect_equal(strsplit(capture_output(print.corpus_frame(x)), "\n")[[1]],
                 lines)
})


test_that("'as_corpus_frame' works on list", {
    actual <- as_corpus_frame(list(text = c("hello", "world")))
    expected <- as_corpus_frame(data.frame(text = as_corpus_text(c("hello", "world"))))
    expect_equal(actual, expected)
})


test_that("'as_corpus_frame' works on character", {
    actual <- as_corpus_frame(c("hello", "world"))
    expected <- as_corpus_frame(data.frame(text = as_corpus_text(c("hello", "world"))))
    expect_equal(actual, expected)
})


test_that("'as_corpus_frame' works on JSON scalar", {
    tmp <- tempfile()
    writeLines(c('"hello"', 'null', '"world"'), tmp)
    obj <- read_ndjson(tmp, simplify = FALSE)
    actual <- as_corpus_frame(obj)
    expected <- as_corpus_frame(data.frame(text = c("hello", NA, "world")))
    expect_equal(actual, expected)
})


test_that("'as_corpus_frame' works on JSON record", {
    tmp <- tempfile()
    writeLines(c('{"text": "hello"}', 'null', '{"a": 1, "text": "world"}'), tmp)
    obj <- read_ndjson(tmp, simplify = FALSE)
    actual <- as_corpus_frame(obj)
    expected <- as_corpus_frame(data.frame(text = c("hello", NA, "world"),
                                           a = c(NA_integer_, NA_integer_, 1L)))
    expect_equal(actual, expected)
})


test_that("'as_corpus_frame' errors on invalid inputs", {
    expect_error(as_corpus_frame.corpus_text("hello"),
                 "argument is not a valid text object")

    expect_error(as_corpus_frame.data.frame(list(text = "hello")),
                 "argument is not a valid data frame")

    expect_error(as_corpus_frame(data.frame(txt = "hello")),
                 "no column named \"text\" in data frame")
})


test_that("'as_corpus_frame' can set row names", {
    x <- data.frame(text = c("a", "b", "c", "d"))
    y <- as_corpus_frame(x, row.names = c("w", "x", "y", "z"))
    expect_equal(y, corpus_frame(text = c("a", "b", "c", "d"),
                                 row.names = c("w", "x", "y", "z")))
})


test_that("'is_corpus_frame' works correctly", {
    expect_false(is_corpus_frame(list(text = "hello")))
    expect_false(is_corpus_frame(data.frame(txt = "hello")))
    expect_false(is_corpus_frame(data.frame(text = "hello",
                                            stringsAsFactors = FALSE)))
    expect_true(is_corpus_frame(data.frame(text = as_corpus_text("hello"))))
})

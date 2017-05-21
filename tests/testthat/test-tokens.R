context("tokens")


test_that("'tokens' splits according to UAX #29", {
    text <- paste0("The quick (\u201cbrown\u201d) fox can\u2019t",
                   " jump 32.3 feet, right?")

    toks <- tokens(text, filter=NULL)

    expect_equal(toks, list(
        c("The", " ", "quick", " ", "(", "\u201c", "brown", "\u201d", ")",
         " ", "fox", " ", "can\u2019t", " ", "jump", " ", "32.3", " ",
         "feet", ",", " ", "right", "?")))
})


test_that("'tokens' normalizes tokens by default", {
    text <- paste0("The quick (\u201cbrown\u201d) fox can\u2019t",
                   " jump 32.3 feet, right?")

    toks <- tokens(text)

    expect_equal(toks, list(
        c("the", "quick", "(", "'", "brown", "'", ")", "fox", "can't",
          "jump", "32.3", "feet", ",", "right", "?")))
})


test_that("'tokens' propagates names if its argument has them", {
    text <- text(a="First sentence.", b="Second sentence!")
    ctext <- c(a="First sentence.", b="Second sentence!")

    toks <- tokens(text)
    expect_equal(toks, list(
        a=c("first", "sentence", "."),
        b=c("second", "sentence", "!")))

    ctoks <- tokens(ctext)
    expect_equal(ctoks, list(
        a=c("first", "sentence", "."),
        b=c("second", "sentence", "!")))
})


test_that("'tokens' works on empty values", {
    expect_equal(tokens(c()), list())
})


test_that("'tokens' works on empty and missing values", {
    expect_equal(tokens(c("1", "2", "", NA, "5")),
                 list("1", "2", character(), NA_character_, "5"))
})


test_that("'tokens' should work on S3 objects", {
    x <- structure(c(a="I LIKE TO SHOUT!! HA HA!",
                     b="There's no need. For that.",
                     c="Why not? :("),
                   class="upper")
    as.character.upper <<- function(x) sapply(unclass(x), toupper)

    x2 <- as.character(x)
    names(x2) <- names(x)

    toks <- tokens(x)
    toks2 <- tokens(x2)
    expect_equal(toks, toks2)
})


test_that("'tokens' can drop symbols", {
    x <- "easy as 1, 2, 3!"
    f <- text_filter(drop_symbol = TRUE)
    expect_equal(tokens(x, f),
                 list(c("easy", "as", "1", NA, "2", NA, "3", NA)))
})


test_that("'tokens' can drop numbers", {
    x <- "easy as 1, 2, 3!"
    f <- text_filter(drop_number = TRUE)
    expect_equal(tokens(x, f),
                 list(c("easy", "as", NA, ",", NA, ",", NA, "!")))
})


test_that("'tokens' can drop letter words", {
    x <- "easy as 1, 2, 3!"
    f <- text_filter(drop_letter = TRUE)
    expect_equal(tokens(x, f),
                 list(c(NA, NA, "1", ",", "2", ",", "3", "!")))
})


test_that("'tokens' can drop tokens", {
    x <- c("Able was I ere I saw Elba.",
           "A man, a plan, a canal: Panama.")
    f <- text_filter(drop = stopwords("english"))
    expect_equal(tokens(x, f),
                 list(c("able", NA, NA, "ere", NA, "saw", "elba", "."),
                      c(NA, "man", ",", NA, "plan", ",", NA, "canal", ":",
                        "panama", ".")))
})


test_that("'tokens' can make drop exceptions", {
    x <- "0, 1, 2, 3, 4, 5"
    f <- text_filter(drop_number = TRUE, drop_except = c("0", "2", "4"))
    expect_equal(tokens(x, f),
                 list(c("0", ",", NA, ",", "2", ",", NA, ",", "4", ",", NA)))
})


test_that("'tokens' can combine two words", {
    x <- c("New York is the Empire State",
           "a new York Street restaurant")
    f <- text_filter(combine = "new york")
    expect_equal(tokens(x, f),
                 list(c("new york", "is", "the", "empire", "state"),
                      c("a", "new york", "street", "restaurant")))
})


test_that("'tokens' can combine three words", {
    x <- c("New York City is the Big Apple")
    f <- text_filter(combine = "new york city")
    expect_equal(tokens(x, f),
                 list(c("new york city", "is", "the", "big", "apple")))
})


test_that("'tokens' can select", {
    x <- c("a b c d e f g h i j",
           "k l m n o p q r s t",
           "u v w x y z")
    f <- text_filter(select = c("a", "e", "i", "o", "u"))
    expect_equal(tokens(x, f),
                 list(c("a", NA, NA, NA, "e", NA, NA, NA, "i", NA),
                      c(NA, NA, NA, NA, "o", NA, NA, NA, NA, NA),
                      c("u", NA, NA, NA, NA, NA)))
})


test_that("'tokens' can select combined tokens", {
    x <- c("New York City, New York",
           "Austin, Texas",
           "Sacramento, California")
    f <- text_filter(combine = "new york city",
                     select = c("new york city", "austin", "sacramento"))
    expect_equal(tokens(x, f),
                 list(c("new york city", NA, NA),
                      c("austin", NA, NA),
                      c("sacramento", NA, NA)))
})

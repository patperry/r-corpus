context("sentences")


test_that("'sentences' splits according to UAX #29 (Examples)", {
    text <- c("He said, 'Are you going?' John Shook his head.",
              "'Are you going?' John asked")

    expect_equal(sentences(text), list(
        text("He said, 'Are you going?' ", "John Shook his head."),
        text("'Are you going?' ", "John asked")))
})


test_that("'sentences' splits according to UAX #29 (Fig. 3)", {
    text <- c("c.d", "3.4", "U.S.", "the resp. leaders are",
              "etc.)' '(the")

    expect_equal(sentences(text), list(
        text("c.d"), text("3.4"), text("U.S."),
        text("the resp. leaders are"), text("etc.)' '(the")))
})


test_that("'sentences' splits according to UAX #29 (Fig. 4)", {
    text <- c("She said 'See spot run.'  John shook his head.",
              "etc.\u5b83\u4eec\u6307",
              "\u7406\u6570\u5b57.\u5b83\u4eec\u6307")

    expect_equal(sentences(text), list(
        text("She said 'See spot run.'  ", "John shook his head."),
        text("etc.", "\u5b83\u4eec\u6307"),
        text("\u7406\u6570\u5b57.", "\u5b83\u4eec\u6307")))
})


test_that("'sentences' cannot handle abbreviations", {
    expect_equal(sentences("Mr. Jones"), list(text("Mr. ", "Jones")))
})


test_that("'sentences' works on length-0 arguments values", {
    expect_equal(sentences(c()), list())
})


test_that("'sentences' works on empty and missing values", {
    expect_equal(sentences(c("1", "2", NA, "", "5")),
                 list(text("1"), text("2"), text(NA), text(), text("5")))
})


test_that("'sentences' propagates names if its argument has them", {
    text <- text(a="First sentence.", b="Second sentence!")
    ctext <- c(a="First sentence.", b="Second sentence!")

    sents <- sentences(text)
    expect_equal(sents, list(a=text("First sentence."),
                             b=text("Second sentence!")))

    # character
    csents <- sentences(ctext)
    expect_equal(csents, list(a=text("First sentence."),
                              b=text("Second sentence!")))
})


test_that("the result of 'sentences' can be serialized", {
    text <- c(a="He said, 'Are you going?' John Shook his head.",
              b="'Are you going?' John asked",
              c="This. Is. A. Long. Sentence!!!",
              d="Why all the shouting??")

    sents <- sentences(text)
    file <- tempfile()
    saveRDS(sents, file)
    sents2 <- readRDS(file)
    expect_equal(sents, sents2)
    file.remove(file)
})


test_that("the result of 'sentences' on JSON data can be serialized", {
    text <- c(a="He said, 'Are you going?' John Shook his head.",
              b="'Are you going?' John asked",
              c="This. Is. A. Long. Sentence!!!",
              d="Why all the shouting??")
    json <- paste0('{"name": "', names(text), '", "text": "', text, '"}')
    file <- tempfile()
    writeLines(json, file)

    data <- read_json(file)
    sents <- sentences(data$text)

    file2 <- tempfile()
    saveRDS(sents, file2)
    sents2 <- readRDS(file2)
    expect_equal(sents, sents2)

    rm("data", "sents", "sents2"); gc(); file.remove(file, file2)
})


test_that("'sentences' should work on S3 objects", {
    x <- structure(c(a="I LIKE TO SHOUT!! HA HA!",
                     b="There's no need. For that.",
                     c="Why not? :("),
                   class="upper")
    as.character.upper <<- function(x) sapply(unclass(x), toupper)

    x2 <- as.character(x)
    names(x2) <- names(x)

    sents <- sentences(x)
    sents2 <- sentences(x2)
    expect_equal(sents, sents2)
})

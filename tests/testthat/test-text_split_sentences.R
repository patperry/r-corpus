context("text_split_sentences")


test_that("'sentences' splits according to UAX #29 (Examples)", {
    text <- c("He said, 'Are you going?' John Shook his head.",
              "'Are you going?' John asked")
    f <- text_filter(sent_suppress = NULL)

    expect_equal(text_split(text, "sentences", filter = f),
        structure(class = c("corpus_frame", "data.frame"),
            data.frame(parent = factor(c("1", "1", "2", "2")),
                       index  = c(1L, 2L, 1L, 2L),
                       text   = as_corpus_text(c("He said, 'Are you going?' ",
                                          "John Shook his head.",
                                          "'Are you going?' ",
                                          "John asked"),
                                        filter = f),
                       row.names = NULL)))
})


test_that("'sentences' splits according to UAX #29 (Fig. 3)", {
    text <- c("c.d", "3.4", "U.S.", "the resp. leaders are",
              "etc.)' '(the")
    f <- text_filter(sent_suppress = NULL)
    expect_equal(text_split(text, "sentences", filter = f),
        structure(class = c("corpus_frame", "data.frame"),
            data.frame(parent = factor(c("1", "2", "3", "4", "5")),
                       index  = c(1L, 1L, 1L, 1L, 1L),
                       text   = as_corpus_text(c("c.d", "3.4", "U.S.",
                                          "the resp. leaders are",
                                          "etc.)' '(the"),
                                        filter = f),
                       row.names = NULL)))
})


test_that("'sentences' splits according to UAX #29 (Fig. 4)", {
    text <- c("She said 'See spot run.'  John shook his head.",
              "etc.\u5b83\u4eec\u6307",
              "\u7406\u6570\u5b57.\u5b83\u4eec\u6307")
    f <- text_filter(sent_suppress = NULL)
    expect_equal(text_split(text, "sentences", filter = f),
        structure(class = c("corpus_frame", "data.frame"),
            data.frame(parent = factor(c("1", "1", "2", "2", "3", "3")),
                       index  = c(1L, 2L, 1L, 2L, 1L, 2L),
                       text   = as_corpus_text(c("She said 'See spot run.'  ",
                                          "John shook his head.",
                                          "etc.",
                                          "\u5b83\u4eec\u6307",
                                          "\u7406\u6570\u5b57.",
                                          "\u5b83\u4eec\u6307"),
                                        filter = f),
                       row.names = NULL)))
})


test_that("'sentences' cannot handle abbreviations without suppressions", {
    f <- text_filter(sent_suppress = NULL)
    expect_equal(text_split("Mr. Jones", "sentences", filter = f),
        structure(class = c("corpus_frame", "data.frame"),
            data.frame(parent = factor(c("1", "1")), index = c(1L, 2L),
                       text = as_corpus_text(c("Mr. ", "Jones"), filter = f),
                       row.names = NULL)))
})


test_that("'sentences' works on length-0 arguments values", {
    expect_equal(text_split(c(), "sentences"),
        structure(class = c("corpus_frame", "data.frame"),
            data.frame(parent = factor(c()), index = integer(),
                       text = as_corpus_text(c()), row.names = NULL)))
})


test_that("'sentences' works on empty and missing values", {
    expect_equal(text_split(c("1", "2", NA, "", "5"), "sentences"),
        structure(class = c("corpus_frame", "data.frame"),
            data.frame(parent = factor(c("1", "2", "4", "5"),
                                       levels = as.character(1:5)),
                       index  = c(1L, 1L, 1L, 1L),
                       text   = as_corpus_text(c("1", "2", "", "5")),
                       row.names = NULL)))
})


test_that("'sentences' uses names if its argument has them", {
    text <- as_corpus_text(c(a="First sentence. Second.", b="Third sentence!"))
    sents <- text_split(text, "sentences")
    expect_equal(sents,
        structure(class = c("corpus_frame", "data.frame"),
            data.frame(parent = factor(c("a", "a", "b"),
                                       levels = c("a", "b")),
                       index = c(1L, 2L, 1L),
                       text = as_corpus_text(c("First sentence. ", "Second.",
                                        "Third sentence!")),
                       row.names = NULL,
                       stringsAsFactors = FALSE)))
})


test_that("the result of 'sentences' can be serialized", {
    text <- c(a="He said, 'Are you going?' John Shook his head.",
              b="'Are you going?' John asked",
              c="This. Is. A. Long. Sentence!!!",
              d="Why all the shouting??")

    sents <- text_split(text, "sentences")
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

    data <- read_ndjson(file)
    sents <- text_split(data$text, "sentences")

    file2 <- tempfile()
    saveRDS(sents, file2)
    sents2 <- readRDS(file2)
    expect_equal(sents, sents2)
})


test_that("'sentences' should work on S3 objects", {
    x <- structure(c(a="I LIKE TO SHOUT!! HA HA!",
                     b="There's no need. For that.",
                     c="Why not? :("),
                   class="upper")
    as.character.upper <<- function(x) sapply(unclass(x), toupper)

    x2 <- as.character(x)
    names(x2) <- names(x)

    sents <- text_split(x, "sentences")
    sents2 <- text_split(x2, "sentences")
    expect_equal(sents, sents2)

    remove("as.character.upper", envir = .GlobalEnv)
})

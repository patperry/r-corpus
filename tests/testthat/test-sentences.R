context("sentences")


test_that("'sentences' splits according to UAX #29 (Examples)", {
    text <- c("He said, 'Are you going?' John Shook his head.",
              "'Are you going?' John asked")

    expect_equal(sentences(text), list(
        c("He said, 'Are you going?' ", "John Shook his head."),
        c("'Are you going?' ", "John asked")))
})


test_that("'sentences' splits according to UAX #29 (Fig. 3)", {
    text <- c("c.d", "3.4", "U.S.", "the resp. leaders are",
              "etc.)' '(the")

    expect_equal(sentences(text), list(
        "c.d", "3.4", "U.S.", "the resp. leaders are", "etc.)' '(the"))
})


test_that("'sentences' splits according to UAX #29 (Fig. 4)", {
    text <- c("She said 'See spot run.'  John shook his head.",
              "etc.\u5b83\u4eec\u6307",
              "\u7406\u6570\u5b57.\u5b83\u4eec\u6307")

    expect_equal(sentences(text), list(
        c("She said 'See spot run.'  ", "John shook his head."),
        c("etc.", "\u5b83\u4eec\u6307"),
        c("\u7406\u6570\u5b57.", "\u5b83\u4eec\u6307")))
})


test_that("'sentences' cannot handle abbreviations", {
    expect_equal(sentences("Mr. Jones"), list(c("Mr. ", "Jones")))
})


test_that("'sentences' works on length-0 arguments values", {
    expect_equal(sentences(c()), list())
})


test_that("'sentences' works on empty and missing values", {
    expect_equal(sentences(c("1", "2", NA, "", "5")),
                 list("1", "2", NA_character_, character(), "5"))
})


test_that("'sentences' propagates names if its argument has them", {
    text <- c(a="First sentence.", b="Second sentence!")

    sents <- sentences(text)

    expect_equal(sents, list( a="First sentence.", b="Second sentence!"))
})


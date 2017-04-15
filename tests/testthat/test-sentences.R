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

    # character
    sents <- sentences(text)
    expect_equal(sents, list(a=text("First sentence."),
                             b=text("Second sentence!")))

    csents <- sentences(ctext)
    expect_equal(csents, list(a=text("First sentence."),
                              b=text("Second sentence!")))
})


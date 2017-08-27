context("foreign")

test_that("'as_text' and 'as_corpus' work on tm VCorpus", {
    content <- list(
        `reut-00001.xml` =
            structure(
                 list(content = "Diamond Shamrock Corp said that...",
                      meta = structure(list(id = "127"),
                                       class = "TextDocumentMeta")),
                 class = c("PlainTextDocument", "TextDocument")),
        `reut-00002.xml` =
            structure(
                 list(content = "OPEC may be forced to meet...",
                      meta = structure(list(id = "144"),
                                       class = "TextDocumentMeta")),
                 class = c("PlainTextDocument", "TextDocument")),
        `reut-00004.xml` =
            structure(
                 list(content = "Texaco Canada said it lowered...",
                      meta = structure(list(id = "191"),
                                       class = "TextDocumentMeta")),
                 class = c("PlainTextDocument", "TextDocument"))
        )
    meta <- structure(list(), class = "CorpusMeta")
    dmeta <- structure(structure(list(), names = character()),
                       row.names = c(NA_integer_, -3L),
                       class = "data.frame")

    crude <- structure(list(content = content, meta = meta, dmeta = dmeta),
                       class = c("VCorpus", "Corpus"))

    text <- as_text(crude)
    expect_equal(text, as_text(c("127" = "Diamond Shamrock Corp said that...",
                                 "144" = "OPEC may be forced to meet...",
                                 "191" = "Texaco Canada said it lowered...")))

    data <- as_corpus(crude)
    expect_equal(data, as_corpus(text))
})


test_that("'as_text' and 'as_corpus' work on quanteda corpus", {
    data <- data.frame(filename = c("reut-00001.xml",
                                    "reut-00002.xml",
                                    "reut-00004.xml"),
                       text = c("Diamond Samrock Corb said that...",
                                "OPEC may be forced to meet",
                                "Texaco Canada said it lowered..."),
                       row.names = c("127", "144", "191"),
                       stringsAsFactors = FALSE)
    qdata <- quanteda::corpus(data)
    quanteda::docnames(qdata) <- rownames(data)

    expect_equal(as_text(qdata), as_text(data))
    expect_equal(as_corpus(data), as_corpus(qdata))
})

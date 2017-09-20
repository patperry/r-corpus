context("foreign")

test_that("'as_corpus_text' and 'as_corpus_frame' work on tm VCorpus", {
    skip_if_not_installed("tm")

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

    text <- as_corpus_text(crude)
    expect_equal(text,
                 as_corpus_text(c("127" = "Diamond Shamrock Corp said that...",
                                  "144" = "OPEC may be forced to meet...",
                                  "191" = "Texaco Canada said it lowered...")))

    data <- as_corpus_frame(crude)
    expect_equal(data, as_corpus_frame(text))
})


test_that("'as_corpus_text' and 'as_corpus_frame' warn if tm VCorpus has duplicate names", {
    skip_if_not_installed("tm")

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
                      meta = structure(list(id = "127"),
                                       class = "TextDocumentMeta")),
                 class = c("PlainTextDocument", "TextDocument"))
        )
    meta <- structure(list(), class = "CorpusMeta")
    dmeta <- structure(structure(list(), names = character()),
                       row.names = c(NA_integer_, -3L),
                       class = "data.frame")

    crude <- structure(list(content = content, meta = meta, dmeta = dmeta),
                       class = c("VCorpus", "Corpus"))

    expect_warning(data <- as_corpus_frame(crude),
                   "renaming rows with duplicate names")
    expect_equal(rownames(data), make.unique(c("127", "144", "127")))

    expect_warning(text <- as_corpus_text(crude),
                   "renaming entries with duplicate names")
    expect_equal(names(text), make.unique(c("127", "144", "127")))
})


test_that("'as_corpus_text' and 'as_corpus_frame' work on quanteda corpus", {
    skip_if_not_installed("quanteda")

    data <- data.frame(filename = c("reut-00001.xml",
                                    "reut-00002.xml",
                                    "reut-00004.xml"),
                       text = c("Diamond Shamrock Corp said that...",
                                "OPEC may be forced to meet",
                                "Texaco Canada said it lowered..."),
                       row.names = c("127", "144", "191"),
                       stringsAsFactors = FALSE)
    qdata <- quanteda::corpus(data)
    quanteda::docnames(qdata) <- rownames(data)

    expect_equal(as_corpus_text(qdata), as_corpus_text(data))
    expect_equal(as_corpus_frame(data), as_corpus_frame(qdata))
})


test_that("'as_corpus_frame' renames docvar named 'text'", {
    skip_if_not_installed("quanteda")

    data <- data.frame(filename = c("reut-00001.xml",
                                    "reut-00002.xml",
                                    "reut-00004.xml"),
                       text = c("Diamond Shamrock Corp said that...",
                                "OPEC may be forced to meet",
                                "Texaco Canada said it lowered..."),
                       row.names = c("127", "144", "191"),
                       stringsAsFactors = FALSE)
    qdata <- quanteda::corpus(data)
    quanteda::docvars(qdata, "text") <- c("a", "b", "c")

    expect_warning(data2 <- as_corpus_frame(qdata),
                   "changing meta-data column name from 'text' to 'text.1'")
    expect_equal(data2$text, as_corpus_text(data$text))
    expect_equal(data2$text.1, c("a", "b", "c"))
})

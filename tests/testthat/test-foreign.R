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

    if (FALSE) {
    # lightweight versions of tm functions, in case tm isn't loaded
    as.character.PlainTextDocument <<- function(x)
        unclass(x)$content
    as.list.VCorpus <<- function(x)
        stats::setNames(unclass(x)$content, names(x))
    names.VCorpus <<- function(x)
        as.character(sapply(unclass(x)$content, function(y) y$meta$id))
    }

    text <- as_text(crude)
    expect_equal(text, as_text(c("127" = "Diamond Shamrock Corp said that...",
                                 "144" = "OPEC may be forced to meet...",
                                 "191" = "Texaco Canada said it lowered...")))

    data <- as_corpus(crude)
    expect_equal(data, as_corpus(text))

    if (FALSE) {
    remove("as.character.PlainTextDocument", "as.list.VCorpus",
           "names.VCorpus", envir = .GlobalEnv)
    }
})

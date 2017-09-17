#!/usr/local/bin/Rscript --vanilla

kinds <- c(de = "german",
           en = "english",
           es = "spanish",
           fr = "french",
           it = "italian",
           pt = "portuguese",
           ru = "russian")

for (lang in names(kinds)) {
    words <- suppressWarnings(corpus:::abbreviations(kinds[[lang]]))
    words <- stringr::str_sort(words, locale = lang)
    assign(paste0("abbreviations_", lang), words)
}

filename <- file.path("..", paste0("abbreviations.rda"))
save(list = paste0("abbreviations_", names(kinds)), file = filename)
tools::resaveRdaFiles(filename)


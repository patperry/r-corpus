#!/usr/local/bin/Rscript --vanilla

kinds <- c(da = "danish",
           de = "german",
           en = "english",
           es = "spanish",
           fi = "finnish",
           fr = "french",
           hu = "hungarian",
           it = "italian",
           nl = "dutch",
           no = "norwegian",
           pt = "portuguese",
           ru = "russian",
           sv = "swedish")

for (lang in names(kinds)) {
    words <- suppressWarnings(corpus:::stopwords(kinds[[lang]]))
    assign(paste0("stopwords_", lang), words)
}

filename <- file.path("..", paste0("stopwords.rda"))
save(list = paste0("stopwords_", names(kinds)), file = filename)
tools::resaveRdaFiles(filename)


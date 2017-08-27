#!/usr/local/bin/Rscript --vanilla

raw <- read.delim(file.path("AFINN", "AFINN-111.txt"),
                  encoding = "UTF-8", header = FALSE, stringsAsFactors = FALSE)
names(raw) <- c("term", "score")
raw$term <- trimws(raw$term)

# exclude multi-type terms
multi <- grepl("[[:space:]]", raw$term)
raw <- raw[!multi,]

# discared row names
rownames(raw) <- NULL

sentiment_afinn <- raw
class(sentiment_afinn) <- c("corpus_frame", "data.frame")

save(sentiment_afinn, file = "../sentiment_afinn.rda")
tools::resaveRdaFiles("../sentiment_afinn.rda")

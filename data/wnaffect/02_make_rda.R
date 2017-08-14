#!/usr/local/bin/Rscript --vanilla

raw <- read.table("wnaffect.tsv", header = TRUE, stringsAsFactors = FALSE)
raw$pos <- factor(raw$pos, levels = c("NOUN", "VERB", "ADJ", "ADV"))
raw$category <- factor(raw$category, levels = unique(raw$category))
raw$emotion <- factor(raw$emotion, levels = unique(raw$emotion))

wnaffect <- raw
class(wnaffect) <- c("corpus_frame", "data.frame")

save(wnaffect, file = "../wnaffect.rda")
tools::resaveRdaFiles("../wnaffect.rda")

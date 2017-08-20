#!/usr/local/bin/Rscript --vanilla

raw <- read.table("wnaffect.tsv", header = TRUE, stringsAsFactors = FALSE)
raw$pos <- factor(raw$pos, levels = c("NOUN", "ADJ", "VERB", "ADV"))
raw$category <- factor(raw$category, levels = unique(raw$category))
raw$emotion <- factor(raw$emotion, levels = unique(raw$emotion))

affect_wordnet <- raw
class(affect_wordnet) <- c("corpus_frame", "data.frame")

save(affect_wordnet, file = "../affect_wordnet.rda")
tools::resaveRdaFiles("../affect_wordnet.rda")


# Based on Haiyan Wang's demo at https://github.com/ropensci/textworkshop17/tree/master/demos/chineseDemo

library("httr")
library("RColorBrewer")
library("readtext")
library("stringi")
library("wordcloud")

set.seed(100) # ensure consistent runs

pause <- function() {
    if (interactive()) {
        readline("\nHit <Return> to continue: ")
    }
}


# 1. Read in Haiyan's stop word list

## download the raw file
csw <- readtext::readtext("https://raw.githubusercontent.com/ropensci/textworkshop17/master/demos/chineseDemo/ChineseStopWords.txt")

## remove whitespace, then extract the comma-separated words
csw <- stringi::stri_replace_all_charclass(csw$text, "\\p{WHITE_SPACE}", "")
stop_words <- stringi::stri_split(csw, fixed = ",")[[1]]

pause()


# 2. Read text from the files in Haiyan's demo directory

## list the files
raw <- httr::GET("https://api.github.com/repos/ropensci/textworkshop17/contents/demos/chineseDemo/govReports")

## get the file names
paths <- sapply(httr::content(raw), function(x) x$path)
names <- tools::file_path_sans_ext(basename(paths))

## download and read in the text
urls <- sapply(httr::content(raw), function(x) x$download_url)
text <- readtext::readtext(urls)$text

pause()


# 3. Tokenize the text using ICU's Chinese word segmenter.

# Corpus does not know how to tokenize languages with no spaces between
# words. Fortunately, the ICU library (used internally by *stringi*) does,
# by using a dictionary of words along with information about their relative
# usage rates. We use this tokenizer, collect a dictionary of the word types,
# and then manually insert zero-width spaces between tokens.

## use the ICU/stringi functions to tokenize the text
toks <- stringi::stri_split_boundaries(text, type = "word")

## get a list of the unique words
dict <- unique(c(toks, recursive = TRUE))

## re-form the text, putting zero-width spaces (U+200B) between tokens
text2 <- sapply(toks, stringi::stri_join, collapse = "\u200B")

pause()


# 4. Analyze the data in Corpus

## put the input text in a data frame for convenience
data <- data.frame(name = names, text = as_text(text2),
                   stringsAsFactors = FALSE)

## specify the token filter; we set 'combine = dict' so that multi-word tokens
## get treated as single entities
f <- token_filter(drop_punct = TRUE, drop = stop_words, combine = dict)

## compute the term occurrence frequencies
stats <- term_counts(data, f)
head(stats, n = 5)

pause()


# 5. Plot the most common terms

## on Mac, set the font family so we can display Chinese characters
font_family <- par("family")
if (Sys.info()["sysname"] == "Darwin") {
    par(family = "STSong") 
}

## make the plot
with(stats, {
    wordcloud::wordcloud(term, count, min.freq = 500,
                         random.order = FALSE, rot.per = 0.25,
                         colors = RColorBrewer::brewer.pal(8, "Dark2"))
})

## restore the old font family
par(family = font_family)

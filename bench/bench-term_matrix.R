library("dplyr", warn.conflicts = FALSE)
library("janeaustenr")
library("magrittr")
library("stringr")

lines <- (austen_books()
          %>% group_by(book)
          %>% mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                            ignore_case = TRUE))))
          %>% ungroup())

text <- c(tapply(lines$text, paste(lines$book, lines$chapter),
                 paste, collapse = "\n"))
if (packageVersion("janeaustenr") < '0.1.5') {
    text <- iconv(text, "latin1", "UTF-8")
}

stop_words <- stopwords("english")

make_matrix <- function(text, ngrams = 1) {
    f <- corpus::token_filter(stemmer = "english", drop_punct = TRUE,
                              drop_number = TRUE, drop = stop_words)
    stats <- corpus::term_counts(text, f, ngrams = ngrams, min = 5)
    x <- corpus::term_matrix(text, f, select = stats$term)
    x
}

results <- microbenchmark::microbenchmark(
    unigrams = make_matrix(text, 1),
    bigrams = make_matrix(text, 1:2),
    trigrams = make_matrix(text, 1:3),
    "4-grams" = make_matrix(text, 1:4),
    "5-grams" = make_matrix(text, 1:5),
    times = 5
)

print(results)

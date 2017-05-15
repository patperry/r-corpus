#  Copyright 2017 Patrick O. Perry.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

stemmers <- c("arabic", "danish", "dutch", "english", "finnish", "french",
	"german", "hungarian", "italian", "norwegian", "porter", "portuguese",
	"romanian", "russian", "spanish", "swedish", "tamil", "turkish")

text_filter <- function(fold_case = TRUE, fold_dash = TRUE, fold_quote = TRUE,
                        map_compatible = TRUE, remove_control = TRUE,
                        remove_ignorable = TRUE, remove_whitespace = TRUE,
                        ignore_empty = TRUE, drop_symbol = FALSE,
                        drop_number = FALSE, drop_letter = FALSE,
                        drop_kana = FALSE, drop_ideo = FALSE,
                        stemmer = NULL)
{
    if (!is.logical(fold_case) || is.na(fold_case)) {
        stop("invalid 'fold_case' value:", fold_case)
    } else if (!is.logical(fold_dash || is.na(fold_dash))) {
        stop("invalid 'fold_dash' value:", fold_dash)
    } else if (!is.logical(fold_quote || is.na(fold_quote))) {
        stop("invalid 'fold_quote' value:", fold_quote)
    } else if (!is.logical(map_compatible || is.na(map_compatible))) {
        stop("invalid 'map_compatible' value:", map_compatible)
    } else if (!is.logical(remove_control || is.na(remove_control))) {
        stop("invalid 'remove_control' value:", remove_control)
    } else if (!is.logical(remove_ignorable || is.na(remove_ignorable))) {
        stop("invalid 'remove_ignorable' value:", remove_ignorable)
    } else if (!is.logical(remove_whitespace || is.na(remove_whitespace))) {
        stop("invalid 'remove_whitespace' value:", remove_whitespace)
    } else if (!is.logical(ignore_empty || is.na(ignore_empty))) {
        stop("invalid 'ignore_empty' value:", ignore_empty)
    } else if (!is.logical(drop_symbol || is.na(drop_symbol))) {
        stop("invalid 'drop_symbol' value:", drop_symbol)
    } else if (!is.logical(drop_number || is.na(drop_number))) {
        stop("invalid 'drop_number' value:", drop_number)
    } else if (!is.logical(drop_letter || is.na(drop_letter))) {
        stop("invalid 'drop_letter' value:", drop_letter)
    } else if (!is.logical(drop_kana || is.na(drop_kana))) {
        stop("invalid 'drop_kana' value:", drop_kana)
    } else if (!is.logical(drop_ideo || is.na(drop_ideo))) {
        stop("invalid 'drop_ideo' value:", drop_ideo)
    } else if (!is.null(stemmer)
               && !(length(stemmer) == 1 && stemmer %in% stemmers)) {
        stop("invalid 'stemmer' value:", stemmer)
    }

    ans <- list(fold_case = fold_case, fold_dash = fold_dash,
                fold_quote = fold_quote, map_compatible = map_compatible,
                remove_control = remove_control,
                remove_ignorable = remove_ignorable,
                remove_whitespace = remove_whitespace,
                ignore_empty = ignore_empty,
                drop_symbol = drop_symbol,
                drop_number = drop_number,
                drop_letter = drop_letter,
                drop_kana = drop_kana,
                drop_ideo = drop_ideo,
                stemmer = stemmer)
    class(ans) <- "text_filter"
    ans
}


print.text_filter <- function(x, ...)
{
    cat("Text filter with the following options:\n\n")
    for (k in names(x)) {
        val <- ifelse(is.null(x[[k]]), "NULL", x[[k]])
        cat(paste0("\t", k, ": ", val, "\n"))
    }
    invisible(x)
}


sentences <- function(x)
{
    x <- as_text(x)
    .Call(C_sentences_text, x)
}


tokens <- function(x, filter = text_filter())
{
    x <- as_text(x)

    if (!is.null(filter) && !inherits(filter, "text_filter")) {
        stop("invalid 'filter' argument")
    }

    .Call(C_tokens_text, x, filter)
}


word_counts <- function(x, filter = text_filter(), ...)
{
    x <- as_text(x)

    if (!is.null(filter) && !inherits(filter, "text_filter")) {
        stop("invalid 'filter' argument")
    }

    stop("not implemented")
}

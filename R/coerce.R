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


as_character_vector <- function(name, value)
{
    if (!(is.null(value) || is.character(value) || is_text(value)
          || all(is.na(value)))) {
        stop(paste0("'", name, "' must be text, a character vector, or NULL"))
    }
    if (is.null(value)) {
        return(NULL)
    }
    # TODO: UTF-8 validation
    as.character(value)
}


as_enum <- function(name, value, choices)
{
    if (!(is.character(value) && length(value) == 1 && !is.na(value))) {
        stop(paste0("'", name, "' should be a character string"))
    }

    i <- pmatch(value, choices, nomatch = 0)
    if (all(i == 0)) {
        stop(paste0("'", name, "' should be one of ",
             paste(dQuote(choices), collapse = ", ")))
    }
    i <- i[i > 0]
    choices[[i]]
}


as_filter <- function(units, filter)
{
    if (units == "sentences") {
        as_sentence_filter(filter)
    } else {
        as_token_filter(filter)
    }
}


as_group <- function(group, n)
{
    if (!is.null(group)) {
        group <- as.factor(group)

        if (length(group) != n) {
            stop(paste0("'group' argument has wrong length ('",
                        length(group), "'; should be '", n, "'"))
        }
    }

    group
}


as_kind <- function(kind)
{
    if (is.null(kind) || all(is.na(kind))) {
        return(NULL)
    } else if (!is.character(kind)) {
        stop(paste("'kind' must be a character vector"))
    }
    kind <- as.character(kind)
    kind <- unique(kind[!is.na(kind)])
    kind
}


as_limit <- function(limit)
{
    if (!((is.na(limit) || is.numeric(limit))
          && length(limit) == 1 && !is.nan(limit))) {
        stop("'limit' should be a numeric value")
    }
    if (is.na(limit)) {
        return(NA)
    }
    if (limit < 0) {
        limit <- 0
    }
    floor(as.double(limit))
}


as_max <- function(name, value)
{
    if (!((is.na(value) || is.numeric(value))
          && length(value) == 1 && !is.nan(value))) {
        stop(paste0("'", name, "' should be a numeric value"))
    }
    if (is.na(value)) {
        value <- Inf
    }
    as.double(value)
}


as_min <- function(name, value)
{
    if (!((is.na(value) || is.numeric(value))
          && length(value) == 1 && !is.nan(value))) {
        stop(paste0("'", name, "' should be a numeric value"))
    }
    if (is.na(value)) {
        value <- -Inf
    }
    as.double(value)
}


as_ngrams <- function(ngrams)
{
    if (is.null(ngrams)) {
        return(NULL)
    }

    if (!is.numeric(ngrams)) {
        stop("'ngrams' argument must be NULL or an integer vector")
    }

    ngrams <- ngrams[is.finite(ngrams) & ngrams >= 1]
    if (length(ngrams) == 0) {
        stop("'ngrams' argument must contain at least one positive value")
    }

    ngrams <- unique(sort(ngrams))

    as.integer(ngrams)
}


as_option <- function(name, value)
{
    if (is.null(value)) {
        return(FALSE)
    }

    if (!(length(value) == 1 && is.logical(value))) {
        stop(paste0("'", name, "' argument must be TRUE or FALSE"))
    }
    as.logical(value)
}


as_size <- function(size)
{
    if (!(is.numeric(size) && length(size) == 1 && !is.na(size))) {
        stop("'size' should be a finite numeric scalar")
    }

    if (is.nan(size) || !(size >= 1)) {
        stop("'size' should be at least 1")
    }

    size <- floor(size)
    as.double(size)
}


as_stemmer <- function(stemmer)
{
    stemmers <- c("arabic", "danish", "dutch", "english", "finnish",
                  "french", "german", "hungarian", "italian", "norwegian",
                  "porter", "portuguese", "romanian", "russian", "spanish",
                  "swedish", "tamil", "turkish")

    if (is.null(stemmer) || length(stemmer) == 1 && is.na(stemmer)) {
        return(NA_character_)
    }

    if (!(length(stemmer) == 1 && is.character(stemmer))) {
        stop(paste0("'stemmer' argument must be a character string,",
                    "NA, or NULL"))
    }
    stemmer <- as.character(stemmer)

    if (!stemmer %in% stemmers) {
        stop(paste0("invalid 'stemmer' argument (\"", stemmer, "\");",
                    " must be NA or one of the following: ",
                    paste0('"', stemmers, '"', collapse=", ")))
    }

    stemmer
}


as_weights <- function(weights, n)
{
    if (!is.null(weights)) {
        weights <- as.numeric(weights)

        if (length(weights) != n) {
            stop(paste0("'weights' argument has wrong length (",
                        length(weights), ", should be ", n, ")"))
        }
        if (anyNA(weights)) {
            stop("'weights' argument contains a missing value");
        }
        if (any(is.nan(weights))) {
            stop("'weights' argument contains a NaN value");
        }
        if (any(is.infinite(weights))) {
            stop("'weights' argument contains an infinite value");
        }
    }

    weights
}

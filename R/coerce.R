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


as_max <- function(max)
{
    if (!((is.na(max) || is.numeric(max))
          && length(max) == 1 && !is.nan(max))) {
        stop("'max' should be a numeric value")
    }
    if (is.na(max)) {
        max <- Inf
    }
    as.double(max)
}


as_min <- function(min)
{
    if (!((is.na(min) || is.numeric(min))
          && length(min) == 1 && !is.nan(min))) {
        stop("'min' should be a numeric value")
    }
    if (is.na(min)) {
        min <- -Inf
    }
    as.double(min)
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
    if (!(length(value) == 1 && is.logical(value))) {
        stop(paste0("'", name, "' argument must be TRUE or FALSE"))
    }
    as.logical(value)
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

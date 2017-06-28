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


as_character_vector <- function(name, value, utf8 = TRUE)
{
    if (!(is.null(value) || is.character(value) || is_text(value)
          || all(is.na(value)))) {
        stop(paste0("'", name, "' should be text, a character vector, or NULL"))
    }
    if (is.null(value)) {
        return(NULL)
    }
    value <- as.character(value)
    if (utf8) {
        if (!isTRUE(msg <- utf8_valid(value))) {
            stop(paste0("'", name, "' is invalid: ", msg))
        }
        value <- enc2utf8(value)
    }
    value
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


as_integer_scalar <- function(name, value)
{
    if (is.null(value)) {
        return(NA_integer_)
    }

    if (!((is.numeric(value) || is.na(value)) && length(value) == 1)) {
        stop(paste0("'", name, "' should be an integer scalar"))
    }
    
    as.integer(value)
}


as_justify <- function(name, value)
{
    as_enum(name, value, c("left", "right", "centre", "none"))
}


as_kind <- function(kind)
{
    if (is.null(kind) || all(is.na(kind))) {
        return(NULL)
    } else if (!is.character(kind)) {
        stop(paste("'kind' should be a character vector"))
    }
    kind <- as_character_vector("kind", kind, utf8 = TRUE)
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
        stop("'ngrams' should be NULL or an integer vector")
    }

    ngrams <- ngrams[is.finite(ngrams) & ngrams >= 1]
    if (length(ngrams) == 0) {
        stop("'ngrams' should contain at least one positive value")
    }

    ngrams <- unique(sort(ngrams))

    as.integer(ngrams)
}


as_option <- function(name, value)
{
    if (is.null(value)) {
        return(FALSE)
    }

    if (!(length(value) == 1 && is.logical(value) && !is.na(value))) {
        stop(paste0("'", name, "' should be TRUE or FALSE"))
    }
    as.logical(value)
}


as_print_gap <- function(name, value)
{
    value <- as_integer_scalar(name, value)
    if (is.na(value)) {
        value <- 1
    } else if (value < 0) {
        stop(paste0("'", name, "' should be non-negative"))
    } else if (value > 1024) {
        stop(paste0("'", name, "' should be less than or equal to 1024"))
    }
    value
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
        stop(paste0("'stemmer' argument should be a character string,",
                    "NA, or NULL"))
    }
    stemmer <- as.character(stemmer)

    if (!stemmer %in% stemmers) {
        stop(paste0("invalid 'stemmer' argument (\"", stemmer, "\");",
                    " should be NA or one of the following: ",
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


utf8_valid <- function(x)
{
    if (is_text(x)) {
        return(TRUE)
    }
    if (!is.character(x) || is.null(x)) {
        x <- as.character(x)
    }
    .Call(C_utf8_valid, x)
}

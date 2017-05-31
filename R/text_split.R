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


abbreviations <- function(kind = "english")
{
    if (is.null(kind)) {
        return(NULL)
    }

    if (!(is.character(kind) && length(kind) == 1 && !is.na(kind))) {
        stop("abbreviations 'kind' must be a character string")
    }

    .Call(C_abbreviations, kind)
}


text_split <- function(x, units = "sentences", size = 1,
                       filter = token_filter(),
                       crlf_break = FALSE,
                       suppress = abbreviations("english"))
{
    x <- as_text(x)
    units <- as_units(units, choices = c("sentences", "tokens"))
    size <- as_size(size)

    if (units == "sentences") {
        crlf_break <- as_crlf_break(crlf_break)
        suppress <- as_suppress(suppress)

        ans <- .Call(C_text_split_sentences, x, size, crlf_break, suppress)
    } else if (units == "tokens") {
        filter <- as_token_filter(filter)

        ans <- .Call(C_text_split_tokens, x, size, filter)
    } else {
        stop(paste0("unrecognized 'units' value: '", units, "'"))
    }

    ans
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


as_units <- function(units, choices)
{
    if (!(is.character(units) && length(units) == 1 && !is.na(units))) {
        stop("'units' should be a character string")
    }

    i <- pmatch(units, choices, nomatch = 0)
    if (all(i == 0)) {
        stop("'units' should be one of ",
             paste(dQuote(choices), collapse = ", "))
    }
    i <- i[i > 0]
    choices[[i]]
}


as_suppress <- function(suppress)
{
    if (!is.null(suppress) && !is.character(suppress)) {
        stop("'suppress' should be NULL or a character vector")
    }

    if (!is.null(suppress)) {
        suppress <- as.character(suppress)
    }

    suppress
}


as_crlf_break <- function(crlf_break)
{
    if (!(is.logical(crlf_break) && length(crlf_break) == 1
          && !is.na(crlf_break))) {
        stop("crlf_break' should be a logical scalar")
    }
    as.logical(crlf_break)
}

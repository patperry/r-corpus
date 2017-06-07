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
    wordlist("abbreviations", kind, C_abbreviations)
}


stopwords <- function(kind = "english")
{
    wordlist("stopwords", kind, C_stopwords)
}


wordlist <- function(type, kind, call)
{
    kind <- as_kind(type, kind)

    words <- character()
    for (k in kind) {
        wk <- .Call(call, k)
        words <- c(words, wk)
    }

    if (length(words) == 0) {
        return(NULL)
    }

    words <- sort(unique(words))
    words
}


as_kind <- function(type, kind)
{
    if (is.null(kind) || all(is.na(kind))) {
        return(NULL)
    } else if (!is.character(kind)) {
        stop(paste(type, "'kind' must be a character vector"))
    }
    kind <- as.character(kind)
    kind <- unique(kind[!is.na(kind)])
    kind
}

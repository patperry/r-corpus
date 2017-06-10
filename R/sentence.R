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


sentence_filter <- function(crlf_break = FALSE,
                            suppress = abbreviations("english"))
{
    ans <- structure(list(),
                     class = c("corpus_sentence_filter", "corpus_filter"))
    ans$crlf_break <- crlf_break
    ans$suppress <- suppress
    ans
}


as_sentence_filter <- function(filter)
{
    if (is.null(filter)) {
        return(NULL)
    }

    if (!is.list(filter)) {
        stop("'filter' argument should be a sentence filter, list, or NULL")
    }

    ans <- structure(list(),
                     class = c("corpus_sentence_filter", "corpus_filter"))
    keys <- names(sentence_filter())
    for (key in keys) {
        ans[[key]] <- filter[[key]]
    }
    ans
}


`$<-.corpus_sentence_filter` <- function(x, name, value)
{
    if (name %in% c("crlf_break")) {
        value <- as_option(name, value)
    } else if (name %in% c("suppress")) {
        value <- as_character_vector(name, value)
    } else {
        stop(paste0("unrecognized sentence filter property: '", name, "'"))
    }
    `$<-.corpus_filter`(x, name, value)
}

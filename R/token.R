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


text_tokens <- function(x, filter = token_filter())
{
    x <- as_text(x)
    filter <- as_token_filter(filter)
    .Call(C_text_tokens, x, filter)
}


token_filter <- function(map_case = TRUE, map_compat = TRUE, map_quote = TRUE,
                         remove_ignorable = TRUE,
                         stemmer = NA, stem_except = drop,
                         combine = abbreviations("english"),
                         drop_letter = FALSE, drop_mark = FALSE,
                         drop_number = FALSE, drop_punct = FALSE,
                         drop_symbol = FALSE, drop_other = FALSE,
                         drop = NULL, drop_except = NULL)
{
    ans <- structure(list(),
                     class = c("corpus_token_filter", "corpus_filter"))

    ans$map_case <- map_case
    ans$map_compat <- map_compat
    ans$map_quote <- map_quote
    ans$remove_ignorable <- remove_ignorable
    ans$stemmer <- stemmer
    ans$stem_except <- stem_except
    ans$combine <- combine
    ans$drop_letter <- drop_letter
    ans$drop_mark <- drop_mark
    ans$drop_number <- drop_number
    ans$drop_symbol <- drop_symbol
    ans$drop_punct <- drop_punct
    ans$drop_other <- drop_other
    ans$drop <- drop
    ans$drop_except <- drop_except

    ans
}


as_token_filter <- function(filter)
{
    if (is.null(filter)) {
        return(NULL)
    }

    if (!is.list(filter)) {
        stop("'filter' argument should be a token filter, list, or NULL")
    }

    ans <- structure(list(),
                     class = c("corpus_token_filter", "corpus_filter"))
    keys <- names(token_filter())
    for (key in keys) {
        ans[[key]] <- filter[[key]]
    }
    ans
}


`$<-.corpus_token_filter` <- function(x, name, value)
{
    if (name %in% c("map_case", "map_compat", "map_quote",
                    "remove_ignorable", "drop_letter", "drop_mark",
                    "drop_number", "drop_symbol", "drop_punct",
                    "drop_other")) {
        value <- as_option(name, value)
    } else if (name %in% c("stem_except", "combine", "drop", "drop_except")) {
        value <- as_character_vector(name, value)
    } else if (name == "stemmer") {
        value <- as_stemmer(value)
    } else {
        stop(paste0("unrecognized token filter property: '", name, "'"))
    }
    `$<-.corpus_filter`(x, name, value)
}


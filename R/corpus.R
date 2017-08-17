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

corpus <- function(..., row.names = NULL, filter = NULL)
{
    x <- data.frame(..., row.names = NULL, check.rows = FALSE,
                    check.names = TRUE, fix.empty.names = TRUE,
                    stringsAsFactors = FALSE)
    as_corpus(x, row.names = row.names, filter = filter)
}


as_corpus <- function(x, row.names = NULL, filter = NULL, ...)
{
    UseMethod("as_corpus")
}


as_corpus.default <- function(x, row.names = NULL, filter = NULL, ...)
{
    x <- as.data.frame(x, row.names = NULL, optional = TRUE,
                       stringsAsFactors = FALSE)
    as_corpus.data.frame(x, row.names = row.names, filter = filter, ...)
}


as_corpus.character <- function(x, row.names = NULL, filter = NULL, ...)
{
    x <- as_text(x)
    as_corpus(x, row.names = row.names, filter = filter, ...)
}


as_corpus.corpus_json <- function(x, row.names = NULL, filter = NULL, ...)
{
    if (length(dim(x)) == 2) {
        x <- as.data.frame(x, text = "text", stringsAsFactors = FALSE)
    } else {
        x <- as_text(x)
    }

    as_corpus(x, row.names = row.names, filter = filter, ...)
}


as_corpus.corpus_text <- function(x, row.names = NULL, filter = NULL, ...)
{
    if (!is_text(x)) {
        stop("argument is not a valid text object")
    }

    x <- data.frame(text = x)
    as_corpus(x, row.names = row.names, filter = filter, ...)
}


as_corpus.data.frame <- function(x, row.names = NULL, filter = NULL, ...)
{
    if (!is.data.frame(x)) {
        stop("argument is not a valid data frame")
    }

    if (!"text" %in% names(x)) {
        stop("no column named \"text\" in data frame")
    }

    x[["text"]] <- as_text(x[["text"]], filter = filter)

    if (!missing(row.names)) {
        row.names(x) <- row.names
    }

    class(x) <- c("corpus_frame", "data.frame")
    x
}


is_corpus <- function(x)
{
    if (!is.data.frame(x)) {
        return(FALSE)
    }
    if (!"text" %in% names(x)) {
        return(FALSE)
    }
    if (!is_text(x[["text"]])) {
        return(FALSE)
    }
    TRUE
}

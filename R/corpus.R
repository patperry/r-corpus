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
    as_corpus(x, filter = filter, row.names = row.names)
}


as_corpus <- function(x, filter = NULL, ..., row.names = NULL)
{
    UseMethod("as_corpus")
}


as_corpus.default <- function(x, filter = NULL, ..., row.names = NULL)
{
    x <- as.data.frame(x, optional = TRUE, stringsAsFactors = FALSE)
    as_corpus(x, filter = filter, ..., row.names = row.names)
}


as_corpus.character <- function(x, filter = NULL, ..., row.names = NULL)
{
    x <- as_text(x)
    as_corpus(x, filter = filter, ..., row.names = row.names)
}


as_corpus.corpus_json <- function(x, filter = NULL, ..., row.names = NULL)
{
    if (length(dim(x)) == 2) {
        x <- as.data.frame(x, text = "text", stringsAsFactors = FALSE)
    } else {
        x <- as_text(x)
    }
    as_corpus(x, filter = filter, ..., row.names = row.names)
}


as_corpus.corpus_text <- function(x, filter = NULL, ..., row.names = NULL)
{
    if (!is_text(x)) {
        stop("argument is not a valid text object")
    }
    x <- data.frame(text = x)
    as_corpus(x, filter = filter, ..., row.names = row.names)
}


as_corpus.data.frame <- function(x, filter = NULL, ..., row.names = NULL)
{
    if (!is.data.frame(x)) {
        stop("argument is not a valid data frame")
    }

    if (!"text" %in% names(x)) {
        stop("no column named \"text\" in data frame")
    }

    with_rethrow({
        filter <- as_filter("filter", filter)
        row.names <- as_names("row.names", row.names, nrow(x))
    })

    x[["text"]] <- as_text(x[["text"]], filter = filter, ...)

    if (!is.null(row.names)) {
        row.names(x) <- row.names
    }

    class(x) <- c("corpus_frame", "data.frame")
    x
}

corpus_with_meta <- function(text, meta, filter = NULL, ..., row.names = NULL)
{
    names <- names(text)
    if (is.null(row.names) && !is.null(names)) {
        if (anyDuplicated(names)) {
            warning("renaming rows with duplicate names")
        }
        names <- make.unique(names)
    }
    names(text) <- NULL

    if ("text" %in% names(meta)) {
        colnames <- names(meta)
        i <- match("text", colnames)
        nm <- make.unique(c("text", colnames))[[i + 1]]
        warning(sprintf("changing meta-data column name from 'text' to '%s'",
                        nm))
        names(meta)[[i]] <- nm
    }
    meta[["text"]] <- text
    row.names(meta) <- names

    as_corpus(meta, filter = filter, ..., row.names = row.names)
}

# tm::Corpus
as_corpus.Corpus <- function(x, filter = NULL, ..., row.names = NULL)
{
    with_tm({
        text <- sapply(x, as.character)
    })
    meta <- as.data.frame(x$dmeta)

    corpus_with_meta(text, meta, filter = filter, ..., row.names = row.names)
}

# quanteda::corpus
as_corpus.corpus <- function(x, filter = NULL, ..., row.names = NULL)
{
    text <- quanteda::texts(x)
    meta <- quanteda::docvars(x)
    corpus_with_meta(text, meta, filter = filter, ..., row.names = row.names)
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

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
    x <- as.data.frame(x, optional = TRUE, stringsAsFactors = FALSE)
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

    with_rethrow({
        row.names <- as_names("row.names", row.names, nrow(x))
        filter <- as_filter("filter", filter)
    })

    x[["text"]] <- as_text(x[["text"]], filter = filter)

    if (!is.null(row.names)) {
        row.names(x) <- row.names
    }

    class(x) <- c("corpus_frame", "data.frame")
    x
}

# tm::Corpus
as_corpus.Corpus <- function(x, row.names = NULL, filter = NULL, ...)
{
    meta <- as.data.frame(x$dmeta)
    with_tm({
        x <- sapply(x, as.character)
    })

    with_rethrow({
        row.names <- as_names("row.names", row.names, length(x))
        filter <- as_filter("filter", filter)
    })
        
    if (is.null(row.names)) {
        row.names <- names(x)
        if (anyDuplicated(row.names)) {
            warning("renaming entries with duplicate names")
        }
        row.names <- make.unique(row.names)
    }

    names(x) <- NULL
    text <- as_text(x, filter = filter)

    if ("text" %in% names(meta)) {
        names <- names(meta)
        i <- match("text", names)
        nm <- make.unique(c("text", names))[[i + 1]]
        warning(sprintf("changing meta-data column name from 'text' to '%s'",
                        nm))
        names(meta)[[i]] <- nm
    }
    meta[["text"]] <- text

    class(meta) <- c("corpus_frame", "data.frame")
    row.names(meta) <- row.names

    meta
}

# quanteda::corpus
as_corpus.corpus <- function(x, row.names = NULL, filter = NULL, ...)
{
    text <- quanteda::texts(x)
    meta <- quanteda::docvars(x)

    with_rethrow({
        row.names <- as_names("row.names", row.names, length(x))
        filter <- as_filter("filter", filter)
    })

    if (is.null(row.names)) {
        row.names <- quanteda::docnames(x)
        if (anyDuplicated(row.names)) {
            warning("renaming entries with duplicate docnames")
        }
        row.names <- make.unique(row.names)
    }

    names(text) <- NULL
    text <- as_text(text, filter = filter)

    if ("text" %in% names(meta)) {
        names <- names(meta)
        i <- match("text", names)
        nm <- make.unique(c("text", names))[[i + 1]]
        warning(sprintf("changing docvar name from 'text' to '%s'", nm))
        names(meta)[[i]] <- nm
    }
    meta[["text"]] <- text

    class(meta) <- c("corpus_frame", "data.frame")
    row.names(meta) <- row.names
    
    meta
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

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

as_corpus_text <- function(x, filter = NULL, ..., names = NULL)
{
    UseMethod("as_corpus_text")
}


as_corpus_text.default <- function(x, filter = NULL, ..., names = NULL)
{
    if (length(dim(x)) > 1) {
        stop("cannot convert multi-dimensional array to text")
    }

    x <- structure(as.character(x), names = names(x))
    as_corpus_text(x, filter = filter, ..., names = names)
}


as_corpus_text.character <- function(x, filter = NULL, ..., names = NULL)
{
    if (length(dim(x)) > 1) {
        stop("cannot convert multi-dimensional array to text")
    }

    with_rethrow({
        x <- as_utf8(x)
    })

    if (is.null(names)) {
        names <- names(x)
        if (anyDuplicated(names)) {
            warning("renaming entries with duplicate names")
            names <- make.unique(names)
        }
    }

    x <- .Call(C_as_text_character, x, NULL)
    as_corpus_text(x, filter = filter, ..., names = names)
}


as_corpus_text.corpus_json <- function(x, filter = NULL, ..., names = NULL)
{
    if (length(dim(x)) == 2) {
        if (!"text" %in% names(x)) {
            stop("no column named \"text\" in JSON object")
        }
        x <- x[["text"]]
    } else {
        x <- .Call(C_as_text_json, x, NULL)
    }
    as_corpus_text(x, filter = filter, ..., names = names)
}


as_corpus_text.corpus_text <- function(x, filter = NULL, ..., names = NULL)
{
    if (!is_corpus_text(x)) {
        stop("argument is not a valid text object")
    }

    with_rethrow({
        filter <- as_filter("filter", filter)
        names <- as_names("names", names, length(x))
    })

    attrs <- attributes(x)
    for (a in names(attrs)) {
        if (!a %in% c("class", "names")) {
            attr(x, a) <- NULL
        }
    }
    attr(x, "class") <- "corpus_text"

    if (!is.null(names)) {
        names(x) <- names
    }
    if (!is.null(filter)) {
        text_filter(x) <- filter
    }

    props <- list(...)
    if (length(props) > 0) {
        pnames <- names(props)
        if (is.null(pnames) || any(pnames == "")) {
            stop("unnamed arguments are not allowed")
        }
        f <- text_filter(x)
        for (name in names(props)) {
            f[[name]] <- props[[name]]
        }
        text_filter(x) <- f
    }

    x
}


as_corpus_text.data.frame <- function(x, filter = NULL, ..., names = NULL)
{
    if (!is.data.frame(x)) {
        stop("argument is not a valid data frame")
    }
    if (!"text" %in% names(x)) {
            stop("no column named \"text\" in data frame")
    }

    text <- x[["text"]]
    if (.row_names_info(x) > 0) {
        names(text) <- row.names(x)
    }

    as_corpus_text(text, filter = filter, ..., names = names)
}


# tm::Corpus
as_corpus_text.Corpus <- function(x, filter = NULL, ..., names = NULL)
{
    with_package("tm", {
        x <- vapply(x, as.character, "")
    })
    as_corpus_text(x, filter = filter, ..., names = names)
}

# quanteda::corpus
as_corpus_text.corpus <- function(x, filter = NULL, ..., names = NULL)
{
    with_package("quanteda", {
        text <- quanteda::texts(x)
    })
    as_corpus_text(text, filter = filter, ..., names = names)
}


is_corpus_text <- function(x)
{
    if (!inherits(x, "corpus_text")) {
        return(FALSE)
    }
    .Call(C_text_valid, x)
}

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

as_text <- function(x, names = NULL, filter = NULL, ...)
{
    UseMethod("as_text")
}


as_text.default <- function(x, names = NULL, filter = NULL, ...)
{
    if (length(dim(x)) > 1) {
        stop("cannot convert multi-dimensional array to text")
    }

    x <- structure(as.character(x), names = names(x))
    as_text.character(x, names = names, filter = filter, ...)
}


as_text.character <- function(x, names = NULL, filter = NULL, ...)
{
    if (length(dim(x)) > 1) {
        stop("cannot convert multi-dimensional array to text")
    }

    with_rethrow({
        x <- as_utf8(x)
        names <- as_names("names", names, length(x))
        filter <- as_filter("filter", filter)
    })

    if (is.null(names)) {
        names <- names(x)
        if (anyDuplicated(names)) {
            warning("renaming entries with duplicate names")
            names <- make.unique(names)
        }
    }

    ans <- .Call(C_as_text_character, x, filter)
    names(ans) <- names
    ans
}


as_text.corpus_json <- function(x, names = NULL, filter = NULL, ...)
{
    n <- if (length(dim(x)) == 2) nrow(x) else length(x)

    with_rethrow({
        names <- as_names("names", names, n)
        filter <- as_filter("filter", filter)
    })

    if (length(dim(x)) == 2) {
        if (!"text" %in% names(x)) {
            stop("no column named \"text\" in JSON object")
        }
        ans <- as_text(x[["text"]], names = names, filter = filter)
    } else {
        ans <- .Call(C_as_text_json, x, filter)
        names(ans) <- names
    }
    ans
}


as_text.corpus_text <- function(x, names = NULL, filter = NULL, ...)
{
    if (!is_text(x)) {
        stop("argument is not a valid text object")
    }

    with_rethrow({
        names <- as_names("names", names, length(x))
        filter <- as_filter("filter", filter)
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

    x
}


as_text.data.frame <- function(x, names = NULL, filter = NULL, ...)
{
    if (!is.data.frame(x)) {
        stop("argument is not a valid data frame")
    }
    if (!"text" %in% names(x)) {
            stop("no column named \"text\" in data frame")
    }

    with_rethrow({
        names <- as_names("names", names, nrow(x))
        filter <- as_filter("filter", filter)
    })

    if (is.null(names)) {
        if (.row_names_info(x) <= 0) {
            names <- NULL
        } else {
            names <- rownames(x)
        }
    }

    as_text(x[["text"]], names = names, filter = filter, ...)
}


# tm::Corpus
as_text.Corpus <- function(x, names = NULL, filter = NULL, ...)
{
    with_tm({
        x <- sapply(x, as.character)
    })
    as_text(x, names = names, filter = filter, ...)
}

# quanteda::corpus
as_text.corpus <- function(x, names = NULL, filter = NULL, ...)
{
    text <- quanteda::texts(x)
    as_text(text, names = names, filter = filter, ...)
}


is_text <- function(x)
{
    if (!inherits(x, "corpus_text")) {
        return(FALSE)
    }
    .Call(C_text_valid, x)
}

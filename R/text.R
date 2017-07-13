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

as_text <- function(x, filter = NULL, ...)
{
    UseMethod("as_text")
}


as_text.default <- function(x, filter = NULL, ...)
{
    as_text(as.character(x), filter = filter, ...)
}


as_text.character <- function(x, filter = NULL, ...)
{
    with_rethrow({
        x <- as_utf8(x)
    })
    x <- .Call(C_as_text_character, x)
    as_text(x, filter = filter, ...)
}


as_text.corpus_json <- function(x, filter = NULL, ...)
{
    if (length(dim(x)) == 2) {
        if (!"text" %in% names(x)) {
            stop("no column named \"text\"")
        }
        as_text(x$text, filter = filter, ...)
    } else {
        x <- .Call(C_as_text_json, x)
        as_text(x, filter = filter, ...)
    }
}


as_text.corpus_text <- function(x, filter = NULL, ...)
{
    if (!is_text(x)) {
        stop("argument is not a valid text object")
    }

    attrs <- attributes(x)
    for (a in names(attrs)) {
        if (!(a %in% c("class", "names"))) {
            attr(x, a) <- NULL
        }
    }
    attr(x, "class") <- "corpus_text"
    x
}


as_text.data.frame <- function(x, filter = NULL, ...)
{
    if (!is.data.frame(x)) {
        stop("argument is not a valid data frame")
    }

    if (!"text" %in% names(x)) {
            stop("no column named \"text\"")
    }
    
    if (.row_names_info(x) <= 0) {
        nm <- NULL
    } else {
        nm <- row.names(x)
    }

    x <- as_text(x[["text"]], ...)
    names(x) <- nm
    x
}


is_text <- function(x)
{
    if (!inherits(x, "corpus_text")) {
        return(FALSE)
    }
    .Call(C_text_valid, x)
}

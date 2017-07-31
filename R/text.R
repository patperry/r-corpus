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

as_text <- function(x, filter = text_filter(x), keep_names = TRUE, ...)
{
    UseMethod("as_text")
}


as_text.default <- function(x, filter = text_filter(x), keep_names = TRUE, ...)
{
    as_text(as.character(x), filter = filter, keep_names = keep_names, ...)
}


as_text.character <- function(x, filter = text_filter(x),
                              keep_names = TRUE, ...)
{
    with_rethrow({
        x <- as_utf8(x)
        filter <- as_filter("filter", filter)
        keep_names <- as_option("keep_names", keep_names)
    })

    .Call(C_as_text_character, x, filter, keep_names)
}


as_text.corpus_json <- function(x, filter = text_filter(x),
                                keep_names = TRUE, ...)
{
    with_rethrow({
        filter <- as_filter("filter", filter)
        keep_names <- as_option("keep_names", keep_names)
    })

    if (length(dim(x)) == 2) {
        if (!"text" %in% names(x)) {
            stop("no column named \"text\" in JSON object")
        }
        as_text(x$text, filter = filter, keep_names = keep_names, ...)
    } else {
        # ignore keep_names; json objects don't have names
        .Call(C_as_text_json, x, filter)
    }
}


as_text.corpus_text <- function(x, filter = text_filter(x),
                                keep_names = TRUE, ...)
{
    if (!is_text(x)) {
        stop("argument is not a valid text object")
    }
    with_rethrow({
        filter <- as_filter("filter", filter)
        keep_names <- as_option("keep_names", keep_names)
    })

    attrs <- attributes(x)
    for (a in names(attrs)) {
        if (!(a %in% c("class", "names"))) {
            attr(x, a) <- NULL
        }
    }
    attr(x, "class") <- "corpus_text"

    if (!is.null(filter)) {
        text_filter(x) <- filter
    }
    if (!keep_names) {
        names(x) <- NULL
    }

    x
}


as_text.data.frame <- function(x, filter = text_filter(x),
                               keep_names = TRUE, ...)
{
    if (!is.data.frame(x)) {
        stop("argument is not a valid data frame")
    }
    with_rethrow({
        filter <- as_filter("filter", filter)
        keep_names <- as_option("keep_names", keep_names)
    })

    if (!"text" %in% names(x)) {
            stop("no column named \"text\" in data frame")
    }
    
    if (.row_names_info(x) <= 0) {
        nm <- NULL
    } else {
        nm <- row.names(x)
    }

    x <- as_text(x[["text"]], filter = filter, keep_names = FALSE, ...)
    if (keep_names) {
        names(x) <- nm
    }
    x
}


is_text <- function(x)
{
    if (!inherits(x, "corpus_text")) {
        return(FALSE)
    }
    .Call(C_text_valid, x)
}

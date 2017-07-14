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

text_filter <- function(x = NULL, ...)
{
    UseMethod("text_filter")
}


text_filter.default <- function(x = NULL, ...,
                                map_case = TRUE, map_compat = TRUE,
                                map_quote = TRUE, remove_ignorable = TRUE,
                                stemmer = NA, stem_dropped = FALSE,
                                stem_except = NULL,
                                combine = abbreviations("english"),
                                drop_letter = FALSE, drop_mark = FALSE,
                                drop_number = FALSE, drop_punct = FALSE,
                                drop_symbol = FALSE, drop_other = FALSE,
                                drop = NULL, drop_except = NULL,
                                crlf_break = FALSE,
                                suppress = abbreviations("english"))
{
    args <- list(...)
    names <- names(args)

    if (length(args) > 0) {
        if (is.null(names) || any(names == "")) {
            stop("unnamed arguments are not allowed")
        }
    }

    ans <- structure(list(), class = c("corpus_text_filter"))
    ans$map_case <- map_case
    ans$map_compat <- map_compat
    ans$map_quote <- map_quote
    ans$remove_ignorable <- remove_ignorable
    ans$stemmer <- stemmer
    ans$stem_dropped <- stem_dropped
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
    ans$crlf_break <- crlf_break
    ans$suppress <- suppress

    for (i in seq_along(args)) {
        name <- names[[i]]
        ans[[name]] <- args[[i]]
    }

    ans
}


text_filter.data.frame <- function(x = NULL, ...)
{
    x <- as_text.data.frame(x)
    text_filter.corpus_text(x, ...)
}


text_filter.corpus_text <- function(x = NULL, ...)
{
    if (!is_text(x)) {
        stop("argument is not a valid text object")
    }

    ans <- unclass(x)$filter
    if (is.null(ans)) {
        ans <- text_filter.default()
    }

    args <- list(...)
    names <- names(args)
    if (length(args) > 0) {
        if (is.null(names) || any(names == "")) {
            stop("unnamed arguments are not allowed")
        }
    }

    for (i in seq_along(args)) {
        prop <- names[[i]]
        ans[[prop]] <- args[[i]]
    }

    ans
}


`text_filter<-` <- function(x, value)
{
    UseMethod("text_filter<-")
}


`text_filter<-.default` <- function(x, value)
{
    stop(sprintf("setting a text filter for objects of class \"%s\" is not allowed",
                 class(x)))
}


`text_filter<-.data.frame` <- function(x, value)
{
    if (!"text" %in% names(x)) {
        stop("no column named \"text\" in data frame")
    }
    text_filter(x$text) <- value
    x
}


`text_filter<-.corpus_text` <- function(x, value)
{
    if (!is_text(x)) {
        stop("argument is not a valid text object")
    }

    with_rethrow({
        value <- as_filter("value", value)
    })

    y <- unclass(x)
    y$filter <- value
    class(y) <- class(x)
    .Call(C_text_filter_update, y)
    y
}


`$<-.corpus_text_filter` <- function(x, name, value)
{
    if (name %in% c("map_case", "map_compat", "map_quote",
                    "remove_ignorable", "drop_letter", "drop_mark",
                    "drop_number", "drop_symbol", "drop_punct",
                    "drop_other", "stem_dropped", "crlf_break")) {
        value <- as_option(name, value)
    } else if (name %in% c("stem_except", "combine", "drop", "drop_except",
                           "suppress")) {
        value <- as_character_vector(name, value)
    } else if (name == "stemmer") {
        value <- as_stemmer(value)
    } else {
        stop(sprintf("unrecognized text filter property: \"%s\"", name))
    }

    y <- unclass(x)
    if (is.null(value)) {
        # setting a list element to NULL is tricky; see
        # http://stackoverflow.com/a/7945259
        y[[name]] <- NA
        y[match(name, names(y))] <- list(NULL)
    } else {
        y[[name]] <- value
    }
    class(y) <- class(x)
    y
}


`[[<-.corpus_text_filter` <- function(x, i, value)
{
    cl <- class(x)[1]
    clname <- gsub("_", " ", gsub("^corpus_", "", cl))

    if (length(i) > 1) {
        stop(paste0("no such ", clname, " property"))
    }
    if (!is.character(i)) {
        name <- names(x)[[i]]
    } else {
        name <- i
    }
    if (is.na(name)) {
        stop(paste0("no such ", clname, " property ('", i, "')"))
    }

    do.call(paste0("$<-.", cl), list(x, name, value))
}


`[<-.corpus_text_filter` <- function(x, i, value)
{
    if (anyNA(i)) {
        stop("NAs are not allowed in subscripted assignments")
    }
    if (!is.character(i)) {
        i <- names(x)[i]
    }

    if (length(value) == 1) {
        value <- rep(value, length(i))
    } else if (length(value) != length(i)) {
        stop("number of items to replace differs from the replacement length")
    }

    for (j in seq_along(i)) {
        key <- i[[j]]
        val <- value[[j]]
        if (!is.na(key)) {
            x[[key]] <- val
        }
    }

    x
}


print.corpus_text_filter <- function(x, ...)
{
    cat("Text filter with the following options:\n\n")
    for (k in names(x)) {
        val <- x[[k]]

        cat(paste0("\t", k, ": "))
        if (is.null(val)) {
            cat("NULL\n")
        } else if (length(val) == 1) {
            cat(paste0(val, "\n"))
        } else {
            utils::str(val, width = getOption("width") - 8 - nchar(k) - 2,
                       give.attr = FALSE)
        }
    }
    invisible(x)
}

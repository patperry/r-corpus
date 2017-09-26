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
                                map_case = TRUE, map_quote = TRUE,
                                remove_ignorable = TRUE,
                                combine = NULL,
                                stemmer = NULL, stem_dropped = FALSE,
                                stem_except = NULL,
                                drop_letter = FALSE, drop_number = FALSE,
                                drop_punct = FALSE, drop_symbol = FALSE,
                                drop = NULL, drop_except = NULL,
                                connector = "_",
                                sent_crlf = FALSE,
                                sent_suppress = corpus::abbreviations_en)
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
    ans$map_quote <- map_quote
    ans$remove_ignorable <- remove_ignorable
    ans$combine <- combine
    ans$stemmer <- stemmer
    ans$stem_dropped <- stem_dropped
    ans$stem_except <- stem_except
    ans$drop_letter <- drop_letter
    ans$drop_number <- drop_number
    ans$drop_punct <- drop_punct
    ans$drop_symbol <- drop_symbol
    ans$drop <- drop
    ans$drop_except <- drop_except
    ans$connector <- connector
    ans$sent_crlf <- sent_crlf
    ans$sent_suppress <- sent_suppress

    for (i in seq_along(args)) {
        name <- names[[i]]
        ans[[name]] <- args[[i]]
    }

    ans
}


text_filter.data.frame <- function(x = NULL, ...)
{
    if (!"text" %in% names(x)) {
        stop("no column named \"text\" in data frame")
    }
    text_filter(x$text, ...)
}


text_filter.corpus_text <- function(x = NULL, ...)
{
    if (!is_corpus_text(x)) {
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
    if (!is_corpus_text(x)) {
        stop("argument is not a valid text object")
    }

    with_rethrow({
        value <- as_filter("value", value)
    })

    if (is.null(value) && is.null(unclass(x)$filter)) {
        return(x)
    }

    value0 <- text_filter(x)
    if (!identical(value, value0)) {
        y <- unclass(x)
        y$handle <- .Call(C_alloc_text_handle)
        y$filter <- value
        class(y) <- class(x)
        x <- y
    }
    x
}


`$<-.corpus_text_filter` <- function(x, name, value)
{
    if (name %in% c("map_case", "map_quote", "remove_ignorable",
                    "drop_letter", "drop_number", "drop_punct",
                    "drop_symbol", "stem_dropped", "sent_crlf")) {
        value <- as_option(name, value)
    } else if (name %in% c("stem_except", "combine", "drop", "drop_except",
                           "sent_suppress")) {
        value <- as_character_vector(name, value)
    } else if (name == "stemmer") {
        value <- as_stemmer(value)
    } else if (name == "connector") {
        value <- as_connector(value)
    } else {
        stop(sprintf("unrecognized text filter property: '%s'", name))
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
    if (length(i) != 1) {
        stop("no such text filter property")
    }
    if (!is.character(i)) {
        i <- as.integer(i)
        if (is.na(i) || i < 1 || i > length(x)) {
            stop("no such text filter property")
        }
        name <- names(x)[[i]]
    } else {
        name <- i
    }

    `$<-.corpus_text_filter`(x, name, value)
}


`[<-.corpus_text_filter` <- function(x, i, value)
{
    if (anyNA(i)) {
        stop("NAs are not allowed in subscripted assignments")
    }
    if (!is.character(i)) {
        with_rethrow({
            i <- names(x)[i]
        })
        if (anyNA(i)) {
            stop("no such text filter property")
        }
    }

    if (length(value) == 1) {
        value <- rep(value, length(i))
    } else if (length(value) != length(i)) {
        stop("number of items to replace differs from the replacement length")
    }

    for (j in seq_along(i)) {
        key <- i[[j]]
        val <- value[[j]]
        x[[key]] <- val
    }

    x
}


print.corpus_text_filter <- function(x, ...)
{
    cat("Text filter with the following options:\n\n")
    for (k in names(x)) {
        val <- x[[k]]

        cat(paste0("    ", k, ": "))
        if (is.null(val)) {
            cat("NULL\n")
        } else if (is.function(val)) {
            cat("<function>\n")
        } else if (length(val) == 1) {
            cat(paste0(val, "\n"))
        } else {
            utils::str(val, width = getOption("width") - 4 - nchar(k) - 2,
                       give.attr = FALSE)
        }
    }
    invisible(x)
}

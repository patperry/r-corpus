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


stemmers <- c("arabic", "danish", "dutch", "english", "finnish", "french",
	"german", "hungarian", "italian", "norwegian", "porter", "portuguese",
	"romanian", "russian", "spanish", "swedish", "tamil", "turkish")


stopwords <- function(kind = "english")
{
    if (is.null(kind)) {
        return(NULL)
    }

    if (!(is.character(kind) && length(kind) == 1)) {
        stop("'kind' must be a character string")
    }

    .Call(C_stopwords, kind)
}


text_filter <- function(map_case = TRUE, map_compat = TRUE,
                        map_dash = TRUE, map_quote = TRUE,
                        remove_control = TRUE, remove_ignorable = TRUE,
                        remove_space = TRUE, ignore_empty = TRUE,
                        stemmer = NULL, stem_except = drop, combine = NULL,
                        drop_symbol = FALSE, drop_number = FALSE,
                        drop_letter = FALSE, drop_kana = FALSE,
                        drop_ideo = FALSE, drop = NULL, drop_except = select,
                        select = NULL)
{
    ans <- structure(list(), class="text_filter")

    ans$map_case <- map_case
    ans$map_compat <- map_compat
    ans$map_dash <- map_dash
    ans$map_quote <- map_quote
    ans$remove_control <- remove_control
    ans$remove_ignorable <- remove_ignorable
    ans$remove_space <- remove_space
    ans$ignore_empty <- ignore_empty
    ans$stemmer <- stemmer
    ans$stem_except <- stem_except
    ans$combine <- combine
    ans$drop_symbol <- drop_symbol
    ans$drop_number <- drop_number
    ans$drop_letter <- drop_letter
    ans$drop_kana <- drop_kana
    ans$drop_ideo <- drop_ideo
    ans$drop <- drop
    ans$drop_except <- drop_except
    ans$select <- select

    ans
}


as_text_filter <- function(x)
{
    if (is.null(x)) {
        return(NULL)
    }

    ans <- structure(list(), class="text_filter")
    keys <- names(text_filter())
    for (key in keys) {
        ans[[key]] <- x[[key]]
    }
    ans
}


`[<-.text_filter` <- function(x, i, value)
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


`$<-.text_filter` <- function(x, name, value)
{
    if (name %in% c("map_case", "map_compat", "map_dash", "map_quote",
                    "remove_control", "remove_ignorable", "remove_space",
                    "ignore_empty", "drop_symbol", "drop_number",
                    "drop_letter", "drop_kana", "drop_ideo")) {
        if (!(is.logical(value) && length(value) == 1 && !is.na(value))) {
            stop(paste0("invalid text_filter '", name, "' property;",
                        " should be TRUE or FALSE"))
        }
    } else if (name %in% c("stem_except", "combine", "drop", "drop_except",
                           "select")) {
        if (!is.null(value) && !is.character(value)) {
            stop(paste0("invalid text_filter '", name, "' property;",
                        " should be a character vector or NULL"))
        }
    } else if (name %in% c("stemmer")) {
        if (!is.null(value) && !(length(value) == 1 && is.character(value))) {
            stop(paste0("invlaid text_filter '", name, "' property;",
                        " should be a character string or NULL"))
        }
    } else {
        stop(paste0("unrecognized text_filter property: '", name, "'"))
    }

    if (name == "stemmer" && !is.null(value) && !(value %in% stemmers)) {
        stop(paste0("unrecognized stemmer: '", value, "'"))
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


`[[<-.text_filter` <- function(x, i, value)
{
    if (length(i) > 1) {
        stop("no such text_filter property")
    }
    if (!is.character(i)) {
        name <- names(x)[[i]]
    } else {
        name <- i
    }
    if (is.na(name)) {
        stop(paste0("no such text_filter property (", i, ")"))
    }

    `$<-.text_filter`(x, name, value)
}


print.text_filter <- function(x, ...)
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


sentences <- function(x)
{
    x <- as_text(x)
    .Call(C_sentences_text, x)
}


tokens <- function(x, filter = text_filter())
{
    x <- as_text(x)
    filter <- as_text_filter(filter)
    .Call(C_tokens_text, x, filter)
}


term_counts <- function(x, filter = text_filter())
{
    x <- as_text(x)
    filter <- as_text_filter(filter)
    stop("not implemented")
}


term_matrix <- function(x, filter = text_filter())
{
    x <- as_text(x)
    filter <- as_text_filter(filter)
    stop("not implemented")
}

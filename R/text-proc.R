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
        stop("stopwords 'kind' must be a character string")
    }

    .Call(C_stopwords, kind)
}


token_filter <- function(map_case = TRUE, map_compat = TRUE, map_quote = TRUE,
                         remove_ignorable = TRUE, ignore_space = TRUE,
                         stemmer = NULL, stem_except = drop, combine = NULL,
                         drop_letter = FALSE, drop_mark = FALSE,
                         drop_number = FALSE, drop_punct = FALSE,
                         drop_symbol = FALSE, drop_other = FALSE,
                         drop = NULL, drop_except = NULL)
{
    ans <- structure(list(), class="corpus_token_filter")

    ans$map_case <- map_case
    ans$map_compat <- map_compat
    ans$map_quote <- map_quote
    ans$remove_ignorable <- remove_ignorable
    ans$ignore_space <- ignore_space
    ans$stemmer <- stemmer
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

    ans
}


as_token_filter <- function(x)
{
    if (is.null(x)) {
        return(NULL)
    }

    ans <- structure(list(), class="corpus_token_filter")
    keys <- names(token_filter())
    for (key in keys) {
        ans[[key]] <- x[[key]]
    }
    ans
}


`[<-.corpus_token_filter` <- function(x, i, value)
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


`$<-.corpus_token_filter` <- function(x, name, value)
{
    if (name %in% c("map_case", "map_compat", "map_quote",
                    "remove_ignorable", "ignore_space", "drop_letter",
                    "drop_mark", "drop_number", "drop_symbol",
                    "drop_punct", "drop_other")) {
        if (!(is.logical(value) && length(value) == 1 && !is.na(value))) {
            stop(paste0("invalid token filter '", name, "' property;",
                        " should be TRUE or FALSE"))
        }
        if (!is.null(value)) {
            value <- as.logical(value)
        }
    } else if (name %in% c("stem_except", "combine", "drop", "drop_except")) {
        if (!is.null(value) && !is.character(value)) {
            stop(paste0("invalid token filter '", name, "' property;",
                        " should be a character vector or NULL"))
        }
        if (!is.null(value)) {
            value <- as.character(value)
        }
    } else if (name %in% c("stemmer")) {
        if (!is.null(value) && !(length(value) == 1 && is.character(value))) {
            stop(paste0("invlaid token filter '", name, "' property;",
                        " should be a character string or NULL"))
        }
        if (!is.null(value)) {
            value <- as.character(value)
        }
    } else {
        stop(paste0("unrecognized token filter property: '", name, "'"))
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


`[[<-.corpus_token_filter` <- function(x, i, value)
{
    if (length(i) > 1) {
        stop("no such token filter property")
    }
    if (!is.character(i)) {
        name <- names(x)[[i]]
    } else {
        name <- i
    }
    if (is.na(name)) {
        stop(paste0("no such token filter property (", i, ")"))
    }

    `$<-.corpus_token_filter`(x, name, value)
}


print.corpus_token_filter <- function(x, ...)
{
    cat("Token filter with the following options:\n\n")
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


tokens <- function(x, filter = token_filter())
{
    x <- as_text(x)
    filter <- as_token_filter(filter)
    .Call(C_tokens_text, x, filter)
}


as_weights <- function(weights, n)
{
    if (!is.null(weights)) {
        weights <- as.numeric(weights)

        if (length(weights) != n) {
            stop(paste0("'weights' argument has wrong length (",
                        length(weights), ", should be ", n, ")"))
        }
        if (anyNA(weights)) {
            stop("'weights' argument contains a missing value");
        }
        if (any(is.nan(weights))) {
            stop("'weights' argument contains a NaN value");
        }
        if (any(is.infinite(weights))) {
            stop("'weights' argument contains an infinite value");
        }
    }

    weights
}


as_group <- function(group, n)
{
    if (!is.null(group)) {
        group <- as.factor(group)

        if (length(group) != n) {
            stop(paste0("'group' argument has wrong length ('",
                        length(group), "'; should be '", n, "'"))
        }
    }

    group
}


as_ngrams <- function(ngrams)
{
    if (is.null(ngrams)) {
        return(NULL)
    }

    if (!is.numeric(ngrams)) {
        stop("'ngrams' argument must be NULL or an integer vector")
    }

    ngrams <- ngrams[is.finite(ngrams) & ngrams >= 1]
    if (length(ngrams) == 0) {
        stop("'ngrams' argument must contain at least one positive value")
    }

    ngrams <- unique(sort(ngrams))

    as.integer(ngrams)
}


as_select <- function(select)
{
    if (is.null(select)) {
        return(NULL)
    }

    if (!is.character(select)) {
        stop(paste0("'select' argument should be a character vector or NULL"))
    }

    as.character(select)
}


as_min <- function(min)
{
    if (!((is.na(min) || is.numeric(min))
          && length(min) == 1 && !is.nan(min))) {
        stop("'min' should be a numeric value")
    }
    if (is.na(min)) {
        min <- -Inf
    }
    as.double(min)
}


as_max <- function(max)
{
    if (!((is.na(max) || is.numeric(max))
          && length(max) == 1 && !is.nan(max))) {
        stop("'max' should be a numeric value")
    }
    if (is.na(max)) {
        max <- Inf
    }
    as.double(max)
}


as_limit <- function(limit)
{
    if (!((is.na(limit) || is.numeric(limit))
          && length(limit) == 1 && !is.nan(limit))) {
        stop("'limit' should be a numeric value")
    }
    if (is.na(limit)) {
        return(NA)
    }
    if (limit < 0) {
        limit <- 0
    }
    floor(as.double(limit))
}


term_counts <- function(x, filter = token_filter(), weights = NULL, 
                        ngrams = NULL, min = NA, max = NA,
                        limit = NA, types = FALSE)
{
    x <- as_text(x)
    filter <- as_token_filter(filter)
    weights <- as_weights(weights, length(x))
    ngrams <- as_ngrams(ngrams)
    min <- as_min(min)
    max <- as_max(max)
    limit <- as_limit(limit)

    if (!(is.logical(types) && length(types) == 1 && !is.na(types))) {
        stop("'types' should be TRUE or FALSE")
    }
    types <- as.logical(types)

    ans <- .Call(C_term_counts_text, x, filter, weights, ngrams, min, max,
                 types)

    # order descending by count, then ascending by term
    o <- order(-ans$count, ans$term)

    # limit output if desired
    if (!is.na(limit) && length(o) > limit) {
        o <- o[seq_len(limit)]
    }

    ans <- ans[o, , drop = FALSE]
    row.names(ans) <- NULL
    ans
}


term_matrix <- function(x, filter = token_filter(), weights = NULL,
                        ngrams = NULL, select = NULL, group = NULL)
{
    x <- as_text(x)
    filter <- as_token_filter(filter)
    weights <- as_weights(weights, length(x))
    ngrams <- as_ngrams(ngrams)
    select <- as_select(select)
    group <- as_group(group, length(x))

    if (is.null(group)) {
        n <- length(x)
    } else {
        n <- nlevels(group)
    }

    mat <- .Call(C_term_matrix_text, x, filter, weights, ngrams, select, group)
    Matrix::sparseMatrix(i = mat$i, j = mat$j, x = mat$count,
                         dims = c(n, length(mat$col_names)),
                         dimnames = list(mat$row_names, mat$col_names),
                         index1 = FALSE, check = FALSE)
}

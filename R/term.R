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


term_stats <- function(x, filter = NULL, ngrams = NULL,
                       min_count = NULL, max_count = NULL,
                       min_support = NULL, max_support = NULL,
                       types = FALSE, subset, ...)
{
    with_rethrow({
        x <- as_corpus_text(x, filter, ...)
        ngrams <- as_ngrams(ngrams)
        min_count <- as_double_scalar("min_count", min_count, TRUE)
        max_count <- as_double_scalar("max_count", max_count, TRUE)
        min_support <- as_double_scalar("min_support", min_support, TRUE)
        max_support <- as_double_scalar("max_support", max_support, TRUE)
        types <- as_option("types", types)
    })

    ans <- .Call(C_term_stats, x, ngrams, min_count, max_count,
                 min_support, max_support, types)

    # order by descending support, then descending count, then ascending term
    o <- order(ans$support, ans$count, ans$term,
               decreasing = c(TRUE, TRUE, FALSE), method = "radix")

    ans <- ans[o, , drop = FALSE]
    row.names(ans) <- NULL

    if (!missing(subset)) {
        e <- substitute(subset)
        r <- eval(e, ans, parent.frame())
        if (!is.logical(r))  {
            stop("'subset' must be logical")
        }
        r <- r & !is.na(r)
        ans <- ans[r, , drop = FALSE]
        row.names(ans) <- NULL
    }

    ans
}


term_matrix_raw <- function(x, filter = NULL, ngrams = NULL, select = NULL,
                            group = NULL, ...)
{
    x <- as_corpus_text(x, filter, ...)
    ngrams <- as_ngrams(ngrams)
    select <- as_character_vector("select", select)
    group <- as_group(group, length(x))

    if (is.null(group)) {
        n <- length(x)
    } else {
        n <- nlevels(group)
    }

    mat <- .Call(C_term_matrix, x, ngrams, select, group)

    if (is.null(select)) {
        # put the terms in lexicographic order
        p <- order(mat$col_names, method = "radix")
        pinv <- integer(length(p))
        pinv[p] <- seq_along(p)

        mat$col_names <- mat$col_names[p]
        mat$j <- pinv[mat$j + 1L] - 1L # 0-based index
    }

    mat$nrow <- n
    mat
}


term_counts <- function(x, filter = NULL, ngrams = NULL, select = NULL,
                        group = NULL, ...)
{
    with_rethrow({
        mat <- term_matrix_raw(x, filter, ngrams, select, group, ...)
    })

    row_names <- mat$row_names
    if (is.null(row_names)) {
        row_names <- as.character(seq_len(mat$nrow))
    }

    row <- structure(as.integer(mat$i + 1L), class = "factor",
                     levels = row_names)
    term <- structure(as.integer(mat$j + 1L), class = "factor",
                      levels = mat$col_names)
    count <- mat$count

    if (is.null(group)) {
        ans <- data.frame(text = row, term, count, stringsAsFactors = FALSE)
    } else {
        ans <- data.frame(group = row, term, count, stringsAsFactors = FALSE)
    }

    # order by term, then text
    o <- order(term, row, method = "radix")
    ans <- ans[o,]
    row.names(ans) <- NULL
    class(ans) <- c("corpus_frame", "data.frame")
    ans
}


term_matrix <- function(x, filter = NULL, ngrams = NULL, select = NULL,
                        group = NULL, transpose = FALSE, ...)
{
    with_rethrow({
        mat <- term_matrix_raw(x, filter, ngrams, select, group, ...)
        transpose <- as_option("transpose", transpose)
    })

    if (!transpose) {
        i <- mat$i
        j <- mat$j
        dims <- c(mat$nrow, length(mat$col_names))
        dimnames <- list(mat$row_names, mat$col_names)
    } else {
        i <- mat$j
        j <- mat$i
        dims <- c(length(mat$col_names), mat$nrow)
        dimnames <- list(mat$col_names, mat$row_names)
    }

    Matrix::sparseMatrix(i = i, j = j, x = mat$count, dims = dims,
                         dimnames = dimnames, index1 = FALSE, check = FALSE)
}

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


term_stats <- function(x, filter = NULL, weights = NULL,
                       ngrams = NULL, min_count = NULL, max_count = NULL,
                       min_support = NULL, max_support = NULL, types = FALSE,
                       subset, ...)
{
    with_rethrow({
        x <- as_corpus_text(x, filter, ...)
        weights <- as_weights(weights, length(x))
        ngrams <- as_ngrams(ngrams)
        min_count <- as_double_scalar("min_count", min_count, TRUE)
        max_count <- as_double_scalar("max_count", max_count, TRUE)
        min_support <- as_double_scalar("min_support", min_support, TRUE)
        max_support <- as_double_scalar("max_support", max_support, TRUE)
        types <- as_option("types", types)
    })

    ans <- .Call(C_term_counts_text, x, weights, ngrams,
                 min_count, max_count, min_support, max_support, types)

    # order by descending support, then descending count, then ascending term
    o <- order(-ans$support, -ans$count, ans$term)

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


term_matrix_raw <- function(x, filter = NULL, weights = NULL, ngrams = NULL,
                            select = NULL, group = NULL, ...)
{
    x <- as_corpus_text(x, filter, ...)
    weights <- as_weights(weights, length(x))
    ngrams <- as_ngrams(ngrams)
    select <- as_character_vector("select", select)
    group <- as_group(group, length(x))

    if (is.null(group)) {
        n <- length(x)
    } else {
        n <- nlevels(group)
    }

    mat <- .Call(C_term_matrix_text, x, weights, ngrams, select, group)

    if (is.null(select)) {
        # put the terms in lexicographic order
        p <- order(mat$col_names)
        pinv <- integer(length(p))
        pinv[p] <- seq_along(p)

        mat$col_names <- mat$col_names[p]
        mat$j <- pinv[mat$j + 1L] - 1L # 0-based index
    }

    mat$nrow <- n
    mat
}


term_matrix <- function(x, filter = NULL, weights = NULL, ngrams = NULL,
                        select = NULL, group = NULL, transpose = FALSE, ...)
{
    with_rethrow({
        mat <- term_matrix_raw(x, filter, weights, ngrams, select, group, ...)
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

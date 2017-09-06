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


term_frame <- function(x, filter = NULL, weights = NULL, ngrams = NULL,
                       select = NULL, group = NULL, ...)
{
    .Deprecated("term_matrix")

    mat <- term_matrix(x, filter = filter, weights = weights, ngrams = ngrams,
                       select = select, group = group, transpose = FALSE,
                       ...)
    mat <- as(mat, "dgTMatrix")

    rn <- rownames(mat)
    if (is.null(rn)) {
        rn <- as.character(seq_len(nrow(mat)))
    }

    row <- structure(as.integer(mat@i + 1L), class = "factor", levels = rn)
    term <- structure(as.integer(mat@j + 1L), class = "factor",
                      levels = colnames(mat))
    count <- mat@x

    if (is.null(group)) {
        ans <- data.frame(text = row, term, count, stringsAsFactors = FALSE)
    } else {
        ans <- data.frame(group = row, term, count, stringsAsFactors = FALSE)
    }

    # order by term, then text
    o <- order(term, row)
    ans <- ans[o,]
    row.names(ans) <- NULL
    class(ans) <- c("corpus_frame", "data.frame")
    ans
}

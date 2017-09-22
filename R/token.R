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


text_tokens <- function(x, filter = NULL, ...)
{
    with_rethrow({
        x <- as_corpus_text(x, filter, ...)
    })
    .Call(C_text_tokens, x)
}


text_ntoken <- function(x, filter = NULL, ...)
{
    with_rethrow({
        x <- as_corpus_text(x, filter, ...)
    })
    .Call(C_text_ntoken, x)
}


text_sub <- function(x, start = 1L, end = -1L, filter = NULL, ...)
{
    with_rethrow({
        x <- as_corpus_text(x, filter, ...)
    })
    n <- length(x)

    if (!(is.numeric(start)
          && (length(dim(start)) <= 1
              || is.matrix(start) && ncol(start) == 2))) {
        stop("'start' must be an integer vector or two-column matrix")
    }

    nstart <- if (is.matrix(start)) nrow(start) else length(start)
    if ((nstart == 0 && n > 0) || (nstart > 0 && n %% nstart != 0)) {
        stop("'start' length does not evenly divide argument length")
    }

    if (is.matrix(start)) {
        if (!missing(end)) {
            warning("'end' argument is ignored when 'start' is a two-column matrix")
        }
        end <- as.integer(start[,2])
        start <- as.integer(start[,1])
    } else {
        start <- as.integer(start)

        if (!(is.numeric(end) && length(dim(end)) <= 1)) {
            stop("'end' must be an integer vector")
        }

        nend <- length(end)
        if ((nend == 0 && n > 0) || (nend > 0 && n %% nend != 0)) {
            stop("'end' length does not evenly divide argument length")
        }
        end <- as.integer(end)
    }

    .Call(C_text_sub, x, start, end)
}

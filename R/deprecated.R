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

as_corpus <- function(x, filter = NULL, ..., row.names = NULL)
{
    .Deprecated("as_corpus_frame")
    as_corpus_frame(x, filter = filter, ..., row.names = row.names)
}


as_text <- function(x, filter = NULL, ..., names = NULL)
{
    .Deprecated("as_corpus_text")
    as_corpus_text(x, filter = filter, ..., names = names)
}


corpus <- function(..., row.names = NULL, filter = NULL)
{
    .Deprecated("corpus_frame")
    call <- match.call()
    call[[1L]] <- quote(corpus_frame)
    eval(call, parent.frame())
}


is_corpus <- function(x)
{
    .Deprecated("is_corpus_frame")
    is_corpus_frame(x)
}


is_text <- function(x)
{
    .Deprecated("is_corpus_text")
    is_corpus_text(x)
}


term_frame <- function(x, filter = NULL, weights = NULL, ngrams = NULL,
                       select = NULL, group = NULL, ...)
{
    .Deprecated("term_counts")
}

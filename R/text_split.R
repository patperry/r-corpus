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


text_split <- function(x, units = "sentences", size = 1, filter = NULL, ...)
{
    with_rethrow({
        x <- as_corpus_text(x, filter, ...)
        units <- as_enum("units", units, choices = c("sentences", "tokens"))
        size <- as_size(size)
    })

    if (units == "sentences") {
        ans <- .Call(C_text_split_sentences, x, size)
    } else {
        stopifnot(units == "tokens")
        ans <- .Call(C_text_split_tokens, x, size)
    }

    ans$parent <- structure(as.integer(ans$parent), class = "factor",
                            levels = labels(x))
    ans
}

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


text_length <- function(x, units = "tokens",
                        filter = if (units == "sentences") sentence_filter()
                                else token_filter(),
                        weights = NULL, group = NULL)
{
    x <- as_text(x)
    units <- as_enum("units", units,
                     choices = c("sentences", "tokens"))
    filter <- as_filter(units, filter)
    weights <- as_weights(weights, length(x))
    group <- as_group(group, length(x))

    if (units == "sentences") {
        ans <- .Call(C_text_length_sentences, x, filter, weights, group)
    } else if (units == "tokens") {
        ans <- .Call(C_text_length_tokens, x, filter, weights, group)
    } else {
        stop(paste0("unrecognized 'units' value: '", units, "'"))
    }

    ans
}


text_ntype <- function(x, filter = token_filter(), weights = NULL,
                       group = NULL)
{
    x <- as_text(x)
    filter <- as_token_filter(filter)
    weights <- as_weights(weights, length(x))
    group <- as_group(group, length(x))
    .Call(C_text_ntype, x, filter, weights, group)
}

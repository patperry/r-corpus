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


text_ntype <- function(x, filter = token_filter(), weights = NULL,
                       group = NULL, collapse = FALSE)
{
    x <- as_text(x)
    filter <- as_token_filter(filter)
    weights <- as_weights(weights, length(x))
    group <- as_group(group, length(x))
    collapse <- as_option("collapse", collapse)
    .Call(C_text_ntype, x, filter, weights, group, collapse)
}


text_types <- function(x, filter = token_filter(), weights = NULL,
                       group = NULL, collapse = FALSE)
{
    x <- as_text(x)
    filter <- as_token_filter(filter)
    weights <- as_weights(weights, length(x))
    group <- as_group(group, length(x))
    collapse <- as_option("collapse", collapse)
    .Call(C_text_types, x, filter, weights, group, collapse)
}

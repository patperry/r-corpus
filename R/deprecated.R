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

term_counts <- function(x, filter = text_filter(x), weights = NULL,
                        ngrams = NULL, min_count = NULL, max_count = NULL,
                        min_support = NULL, max_support = NULL, types = FALSE)
{
    .Deprecated("term_stats")
    term_stats(x = x, filter = filter, weights = weights, ngrams = ngrams,
               min_count = min_count, max_count = max_count,
               min_support = min_support, max_support = max_support,
               types = types)
}

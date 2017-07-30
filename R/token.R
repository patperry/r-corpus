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


text_tokens <- function(x, filter = text_filter(x))
{
    with_rethrow({
        x <- as_text(x, filter = filter)
    })
    .Call(C_text_tokens, x)
}


text_ntoken <- function(x, filter = text_filter(x))
{
    with_rethrow({
        x <- as_text(x, filter = filter)
    })
    .Call(C_text_ntoken, x)
}


text_length <- function(x, filter = text_filter(x))
{
    with_rethrow({
        x <- as_text(x, filter = filter)
    })
    .Call(C_text_length, x)
}


text_sub <- function(x, start = 1L, end = -1L, filter = text_filter(x))
{
    with_rethrow({
        x <- as_text(x, filter = filter)
    })

    # TODO: handle start, end args

    .Call(C_text_sub, x, start, end)
}

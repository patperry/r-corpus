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


text_split <- function(x, units = "sentences", size = 1,
                       filter = token_filter(),
                       crlf_break = FALSE,
                       suppress = abbreviations("english"))
{
    x <- as_text(x)
    units <- as_enum("units", units, choices = c("sentences", "tokens"))
    size <- as_size(size)

    if (units == "sentences") {
        crlf_break <- as_option("crlf_break", crlf_break)
        suppress <- as_character_vector("suppress", suppress)

        ans <- .Call(C_text_split_sentences, x, size, crlf_break, suppress)
    } else if (units == "tokens") {
        filter <- as_token_filter(filter)

        ans <- .Call(C_text_split_tokens, x, size, filter)
    } else {
        stop(paste0("unrecognized 'units' value: '", units, "'"))
    }

    ans
}

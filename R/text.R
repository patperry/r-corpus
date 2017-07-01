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

as_text <- function(x, ...)
{
    UseMethod("as_text")
}


as_text.character <- function(x, ...)
{
    with_rethrow({
        x <- as_utf8(x)
        .Call(C_as_text_character, x)
    })
}


as_text.default <- function(x, ...)
{
    if (is_text(x)) {
        attrs <- attributes(x)
        for (a in names(attrs)) {
            if (!(a %in% c("class", "names"))) {
                attr(x, a) <- NULL
            }
        }
        attr(x, "class") <- "corpus_text"
        x
    } else if (is.data.frame(x)) {
        if (.row_names_info(x) <= 0) {
            nm <- NULL
        } else {
            nm <- row.names(x)
        }

        # find the columns with type 'text'
        text_cols <- sapply(x, is_text)
        ntext <- sum(text_cols == TRUE)

        if ("text" %in% names(x)) {
            x <- as_text(x$text)
        } else if (ntext == 0) {
            stop("no column named 'text', and no columns of type 'text'")
        } else if (ntext > 1) {
            stop("no column named 'text', and multiple columns of type 'text'")
        } else {
            x <- as_text(x[[which(text_cols)]])
        }

        names(x) <- nm
        x
    } else {
        nm <- names(c(x))
        x <- as_text(as.character(x, ...))
        names(x) <- nm
        x
    }
}


is_text <- function(x)
{
    if (!inherits(x, "corpus_text")) {
        return(FALSE)
    }
    .Call(C_text_valid, x)
}

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


# converts a character vector from its declared encoding to UTF-8
as_utf8 <- function(x)
{
    .Call(C_utf8_coerce, x)
}

# encode an R character string in a form suitable for display
# in the current locale (determined by LC_CTYPE)
utf8_encode <- function(x, display = FALSE)
{
    utf8 <- (Sys.getlocale("LC_CTYPE") != "C")
    .Call(C_utf8_encode, x, display, utf8)
}


utf8_format <- function(x, trim = FALSE, chars = NULL, justify = "left",
                        width = NULL, na.encode = TRUE, ...)
{
    if (is.null(x)) {
        return(NULL)
    }

    with_rethrow({
        trim <- as_option("trim", trim)
        if (is.null(chars)) {
            chars <- .Machine$integer.max
        } else {
            chars <- as_integer_scalar("chars", chars)
        }
        justify <- as_justify("justify", justify)
        width <- as_integer_scalar("width", width)
        na.encode <- as_option("na.encode", na.encode)

        utf8 <- Sys.getlocale("LC_CTYPE") != "C"
        .Call(C_utf8_format, x, trim, chars, justify, width, na.encode, utf8)
    })
}


# test whether the elements can be converted to valid UTF-8
utf8_valid <- function(x)
{
    .Call(C_utf8_valid, x)
}

# gets the width; invalid sequences (?)
utf8_width <- function(x, encode = FALSE)
{
    if (encode) {
        x <- utf8_encode(x)
    } else {
        x <- as_utf8(x)
    }
    .Call(C_utf8_width, x)
}

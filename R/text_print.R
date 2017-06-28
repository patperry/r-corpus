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

format.corpus_text <- function(x, trim = FALSE, chars = 45L, justify = "left",
                               width = NULL, na.encode = TRUE, ...)
{
    with_rethrow({
        x <- as_text(x)
        trim <- as_option("trim", trim)
        chars <- as_integer_scalar("chars", chars)
        justify <- as_justify("justify", justify)
        width <- as_integer_scalar("width", width)
        na.encode <- as_option("na.encode", na.encode)
        utf8 <- Sys.getlocale("LC_CTYPE") != "C"
        .Call(C_format_text, x, trim, chars, justify, width, na.encode, utf8)
    })
}


print.corpus_text <- function(x, max = 6L, ...)
{
    if (length(x) == 0) {
        cat("text(0)\n")
        return(invisible(x))
    }

    if (is.null(max) || is.na(max)) {
        max <- getOption("max.print")
    }

    if (is.na(max) || length(x) <= max) {
        xsub <- x
        nextra <- 0
    } else {
        xsub <- x[seq_len(max)]
        nextra <- length(x) - max
    }

    str <- format(xsub, na.encode = FALSE, ...)
    nm <- names(str)

    if (is.null(nm)) {
        lab <- format(paste0("[", seq_along(str), "]"), justify="right")
    } else {
        lab <- format(nm, justify="left")
    }
    for (i in seq_along(str)) {
        cat(lab[[i]], " ", encodeString(str[[i]]), "\n", sep="")
    }

    if (nextra > 0) {
        cat(paste0(paste0(rep(" ", max(nchar(lab))), collapse=""), "\u22ee\n"))
        cat("(", length(x), " entries total)\n", sep="")
    }

    invisible(x)
}

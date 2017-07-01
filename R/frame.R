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

format.corpus_frame <- function(x, ..., display = FALSE, justify = "none")
{
    # TODO: format character specially
    format.data.frame(x, ..., display = display, justify = justify)
}


print.corpus_frame <- function(x, chars = NULL, digits = NULL,
                               quote = FALSE, na.print = NULL,
                               print.gap = NULL, right = TRUE,
                               row.names = TRUE, max = NULL,
                               display = TRUE, ...)
{
    n <- nrow(x)

    chars <- as_integer_scalar("chars", chars, null = NULL)
    digits <- as_digits("digits", digits)
    quote <- as_option("quote", quote)
    na.print <- as_na_print("na.print", na.print)
    print.gap <- as_print_gap("print_gap", print.gap)
    right <- as_option("right", right)
    if (!isTRUE(row.names)) {
        if (identical(row.names, FALSE)) {
            row.names <- rep("", n)
        } else {
            row.names <- as_names("row.names", row.names)
        }
    }
    max <- as_max_print("max", max)
    if (is.null(max)) {
        max <- getOption("max.print")
    }

    if (length(x) == 0) {
        cat(sprintf(ngettext(n, "data frame with 0 columns and %d row", 
            "data frame with 0 columns and %d rows"), n), "\n", 
            sep = "")
    }

    fmt <- format.corpus_frame(x, chars = chars, digits = digits,
                               na.encode = FALSE, display = display)
    m <- as.matrix(fmt)
    if (!(is.matrix(m) && storage.mode(m) == "character")) {
        stop("'format' returned a malformed value")
    }
    if (!isTRUE(row.names)) {
        rownames(m) <- row.names
    }

    nr <- nrow(m)
    nc <- ncol(m)
    len <- length(m)
    if (trunc <- (len > max)) {
        limit <- max(1, max %/% nc)
        m <- m[1:limit, , drop = FALSE]
    }

    dims <- dimnames(m)
    udims <- lapply(dims, function(x) if (is.null(x)) NULL else utf8_encode(x))
    um <- utf8_encode(m)
    dimnames(um) <- udims

    if (is.null(na.print)) {
        na.print <- ifelse(quote, "<NA>", "NA")
    }
    na.print <- utf8_encode(na.print)

    if (is.null(print.gap)) {
        print.gap <- 1L
    }

    width <- getOption("width")

    .Call(C_print_frame, um, quote, na.print, print.gap, right, width)

    if (trunc) {
        ellipsis <- ifelse(Sys.getlocale("LC_CTYPE") == "C", "...", "\u22ee")
        cat(sprintf("%s\n(%d rows total)\n", ellipsis, nr))
    }

    invisible(x)
}

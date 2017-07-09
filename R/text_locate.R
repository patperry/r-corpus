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


text_count <- function(x, terms, filter = token_filter())
{
    x <- as_text(x)
    terms <- as_character_vector("terms", terms)
    filter <- as_token_filter(filter)
    .Call(C_text_count, x, terms, filter)
}


text_detect <- function(x, terms, filter = token_filter())
{
    x <- as_text(x)
    terms <- as_character_vector("terms", terms)
    filter <- as_token_filter(filter)
    .Call(C_text_detect, x, terms, filter)
}


text_locate <- function(x, terms, filter = token_filter())
{
    x <- as_text(x)
    nm <- names(x)
    terms <- as_character_vector("terms", terms)
    filter <- as_token_filter(filter)
    ans <- .Call(C_text_locate, x, terms, filter)
    if (!is.null(nm)) {
        ans$text <- nm[ans$text]
    }
    ans
}


format.corpus_text_locate <- function(x, width = getOption("width"),
                                      print.gap = NULL, ...,
                                      display = FALSE, justify = "left")
{
    width <- as_integer_scalar("width", width)
    print.gap <- as_print_gap("print.gap", print.gap)
    justify <- as_justify("justify", justify)

    if (is.null(print.gap)) {
        print.gap <- 1
    }

    nctx <- 0
    rval <- list()
    colwidths <- c()
    charmax <- .Machine$integer.max
    names <- names(x)

    for (i in seq_along(x)) {
        nm <- names[[i]]
        if (nm %in% c("before", "after")) {
            rval[[i]] <- NA # set to NA and deal with this later
            colwidths[[i]] <- 0
            nctx <- nctx + 1
        } else {
            w = utf8_width(nm)
            j <- justify
            if (nm == "text") {
                # use as_text to format an integer text id like
                # a character label
                rval[[i]] <- format(as_text(x[[i]]), width = 4,
                                    chars = charmax, display = display,
                                    justify = j)
            } else if (nm == "instance") {
                j <- "centre"
                rval[[i]] <- format(as_text(x[[i]]), width = 8,
                                    chars = charmax, display = display,
                                    justify = j)
            } else {
                rval[[i]] = format(x[[i]], width = w, ...,
                                   display = display, justify = j)
            }
            colwidths[[i]] <- max(w, utf8_width(rval[[i]]))
            names[[i]] <- format(nm, width = colwidths[[i]],
                                 chars = charmax, justify = j)
        }
    }

    row_names <- format(as_text(rownames(x)), width = 0, chars = charmax,
                        display = display, jusitfy = "left")
    colwidths <- c(colwidths, max(0, utf8_width(row_names)))

    extra <- width - sum(colwidths) - print.gap * (length(colwidths) - 1)
    ctxwidth <- max(12, extra / max(1, nctx))
    ellipsis <- ifelse(Sys.getlocale("LC_CTYPE") == "C", 3, 1)

    if ("before" %in% names) {
        i <- match("before", names)
        rval[[i]] <- format(x[[i]], chars = floor(ctxwidth) - ellipsis,
                            display = display, justify = "right")
        names[[i]] <- format("before", width = max(0, utf8_width(rval[[i]])),
                             chars = charmax, justify = "left")
    }

    if ("after" %in% names) {
        i <- match("after", names)
        rval[[i]] <- format(x[[i]], chars = ceiling(ctxwidth) - ellipsis,
                            display = display, justify = "left")
        names[[i]] <- format("after", width = max(0, utf8_width(rval[[i]])),
                             chars = charmax, justify = "right")
    }
    names(rval) <- names

    for (i in seq_along(rval)) {
        oldClass(rval[[i]]) <- "AsIs"
    }

    ans <- structure(rval, class = c("corpus_frame", "data.frame"),
                     row.names = row_names)
    ans
}


print.corpus_text_locate <- function(x, print.gap = NULL, display = TRUE, ...)
{
    fmt <- format.corpus_text_locate(x, print.gap = print.gap,
                                     display = display, ...)
    print.corpus_frame(fmt, chars = .Machine$integer.max,
                       print.gap = print.gap)
    invisible(x)
}

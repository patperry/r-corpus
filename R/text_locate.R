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
                                      ..., justify = "none")
{
    nctx <- 0
    rval <- list()
    colwidths <- c()

    for (i in names(x)) {
        if (i %in% c("before", "after")) {
            rval[[i]] <- NA # set to NA and deal with this later
            colwidths[[i]] <- 0
            nctx <- nctx + 1
        } else {
            if (i == "text") {
                rval[[i]] <- format(as.character(x[[i]]), width = 4,
                                    justify = justify)
            } else if (i == "instance") {
                rval[[i]] <- format(x[[i]], width = 8, justify = "centre")
            } else {
                rval[[i]] = format(x[[i]], width = nchar(i, "width"),
                                   ..., justify = justify)
            }
            colwidths[[i]] <- max(nchar(i, "width"), nchar(rval[[i]], "width"))
        }
    }

    row_names <- format(rownames(x), jusitfy = "left", width = 1)
    colwidths <- c(colwidths, max(1, nchar(row_names, "width")))

    ctxwidth <- max(12, floor((width
                               - sum(colwidths)
                               - length(colwidths)) / max(1, nctx)))

    if (!is.null(x$before)) {
        rval[["before"]] <- format(x$before, chars = ctxwidth,
                                   justify = "right")
    }

    if (!is.null(x$after)) {
        rval[["after"]] <- format(x$after, chars = ctxwidth,
                                  justify = "left")
    }

    for (i in seq_along(rval)) {
        oldClass(rval[[i]]) <- "AsIs"
    }

    as.data.frame(rval, row.names = row_names)
}


print.corpus_text_locate <- function(x, ...)
{
    print(format(x))
    invisible(x)
}

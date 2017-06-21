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


format.corpus_text_locate <- function(x, width = getOption("width"), ...)
{
    row_names <- format(rownames(x), jusitfy = "left")
    rval <- list(text = format(x$text, justify = "left"),
                 term = format(x$term, justify = "left"),
                 before = NULL,
                 instance = format(x$instance, justify = "centre"),
                 after = NULL)
    colwidths <- sapply(rval, function(col) max(c(0, nchar(col))))
    for (col in names(colwidths)) {
        colwidths[[col]] <- max(nchar(col), colwidths[[col]])
    }
    colwidths[["names"]] <- max(c(1, nchar(row_names)))

    cwidth <- floor((width - sum(colwidths) - length(rval)) / 2)
    if (cwidth < 10) {
        cwidth <- 10
    }

    rval[["before"]] <- format(x$before, justify = "right", truncate = cwidth - 1)
    rval[["after"]] <- format(x$after, justify = "left", truncate = cwidth - 1)

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

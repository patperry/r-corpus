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

text <- function(...)
{
    x <- c(...)
    nm <- names(x)
    x <- as.text(x)
    names(x) <- nm
    x
}

as.text <- function(x, ...)
{
    UseMethod("as.text")
}

as.text.default <- function(x, ...)
{
    x <- .Call(C_coerce_text, x)
    names(x) <- NULL
    x
}

as.text.text <- function(x, ...)
{
    x <- list(handle=unclass(x)$handle)
    class(x) <- "text"
    x
}

is.text <- function(x)
{
    UseMethod("is.text")
}

is.text.default <- function(x)
{
    inherits(x, "text")
}

length.text <- function(x)
{
    .Call(C_length_text, x)
}

dim.text <- function(x)
{
    NULL
}

names.text <- function(x)
{
    unclass(x)$names
}

`names<-.text` <- function(x, value)
{
    if (!is.null(value)) {
        value <- as.character(value)
        if (length(value) != length(x)) {
            stop(paste0("names attribute [", length(value), "]",
                        " must be the same length as the text object [",
                        length(x), "]"))
        }
    }
    y <- list(handle=unclass(x)$handle, names=value)
    attrs <- attributes(x)
    attrs[["names"]] <- NULL

    for (a in names(attrs)) {
        attr(y, a) <- attrs[[a]]
    }

    y
}

`[.text` <- function(x, i)
{
    ind <- seq_along(x)
    names(ind) <- names(x)
    .Call(C_subset_text, x, ind[i])
}

`$.text` <- function(x, name)
{
    stop("$ operator is invalid for text objects")
}


`$<-.text` <- function(x, name, value)
{
    stop("$<- operator is invalid for text objects")
}


`[<-.text` <- function(x, i, value)
{
    stop("[<- operator is invalid for text objects")
}


`[[.text` <- function(x, i)
{
    ind <- seq_along(x)
    names(ind) <- names(x)
    as.character(x[ind[[i]]])
}


`[<-.text` <- function(x, i, value)
{
    stop("[[<- operator is invalid for text objects")
}


format.text <- function(x, nchar_max = 65, suffix = "\u2026", ...)
{
    if (length(x) == 0) {
        str <- character()
    } else if (is.null(nchar_max)) {
        str <- as.character(x)
        names(str) <- names(x)
    } else {
        str <- as.character(x)
        names(str) <- names(x)
        len <- nchar(str)
        long <- len >= nchar_max + 1
        str[long] <- paste0(substr(str[long], 1, nchar_max), suffix)
    }
    format(str, ...)
}

print.text <- function(x, justify = "none", print_max = 6L, ...)
{
    if (is.null(print_max) || is.na(print_max) || length(x) <= print_max) {
        xsub <- x
        nextra <- 0
    } else {
        xsub <- x[seq_len(print_max)]
        nextra <- length(x) - print_max
    }

    if (length(xsub) == 0) {
        cat("text(0)\n")
    } else if (print_max > 0) {
        print(format(xsub, justify = justify, na.encode = FALSE, ...))
        if (nextra > 0) {
            cat(paste0(paste0(rep(" ", ceiling(log10(print_max + 1))),
                              collapse=""),
                       "\u22ee\n(", length(x), " rows total)\n"))
        }
    } else {
        cat(paste0("(", length(x), " rows total)\n"))
    }

    invisible(x)
}

summary.text <- function(object, ...)
{
    value <- c(Length = length(object), Class = "text", Mode = "character")
    class(value) <- c("summaryDefault", "table")
    value
}

as.character.text <- function(x, ...)
{
    .Call(C_as_character_text, x)
}

as.complex.text <- function(x, ...)
{
    as.complex(as.character(x, ...))
}

as.double.text <- function(x, ...)
{
    as.double(as.character(x, ...))
}

as.integer.text <- function(x, ...)
{
    as.integer(as.character(x, ...))
}

as.logical.text <- function(x, ...)
{
    as.logical(as.character(x, ...))
}

as.Date.text <- function(x, format, ...)
{
    as.Date(as.character(x), format, ...)
}

is.character.text <- function(x)
{
    TRUE
}

all.equal.text <- function(target, current, ...)
{
    if (!is.text(current)) {
        return(c(paste("Modes: text,", mode(current)),
                 paste("target is text, current is", mode(current))))
    }

    nt <- names(target)
    target <- as.character(target)
    names(target) <- nt

    nc <- names(current)
    current <- as.character(current)
    names(current) <- nc

    all.equal(target, current, ...)
}

is.na.text <- function(x)
{
    .Call(C_is_na_text, x)
}

anyNA.text <- function(x, recursive = FALSE)
{
    .Call(C_anyNA_text, x)
}

Ops.text <- function(e1, e2)
{
    if (nargs() == 1)
        stop(gettextf("unary %s not defined for \"text\" objects",
            .Generic), domain = NA)
    boolean <- switch(.Generic, `<` = , `>` = , `==` = , `!=` = ,
        `<=` = , `>=` = TRUE, FALSE)
    if (!boolean)
        stop(gettextf("%s not defined for \"text\" objects",
            .Generic), domain = NA)
    e1 <- as.character(e1)
    e2 <- as.character(e2)
    NextMethod(.Generic)
}

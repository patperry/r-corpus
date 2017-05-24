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
    as_text(x)
}

as_text <- function(x, ...)
{
    UseMethod("as_text")
}

as_text.character <- function(x, ...)
{
    .Call(C_as_text_character, c(x)) # c(x) drops attributes, keeps names
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
    UseMethod("is_text")
}


is_text.default <- function(x)
{
    inherits(x, "corpus_text")
}


length.corpus_text <- function(x)
{
    .Call(C_length_text, x)
}


dim.corpus_text <- function(x)
{
    NULL
}


names.corpus_text <- function(x)
{
    unclass(x)$names
}


`names<-.corpus_text` <- function(x, value)
{
    if (!is.null(value)) {
        value <- as.character(value)
        if (length(value) != length(x)) {
            stop(paste0("names attribute [", length(value), "]",
                        " must be the same length as the text object [",
                        length(x), "]"))
        }
    }

    y <- unclass(x)
    y$names <- value
    class(y) <- class(x)
    y
}


`[.corpus_text` <- function(x, i)
{
    index <- seq_along(x)
    names(index) <- names(x)
    i <- index[i]

    y <- unclass(x)
    y$handle <- .Call(C_subset_text_handle, y$handle, as.double(i))
    y$table <- y$table[i,]
    y$names <- y$names[i]
    class(y) <- class(x)
    y
}


`[[.corpus_text` <- function(x, i)
{
    index <- seq_along(x)
    names(index) <- names(x)
    i <- index[[i]]
    as.character(x[i])
}


`$.corpus_text` <- function(x, name)
{
    stop("$ operator is invalid for text objects")
}


`$<-.corpus_text` <- function(x, name, value)
{
    stop("$<- operator is invalid for text objects")
}


`[<-.corpus_text` <- function(x, i, value)
{
    stop("[<- operator is invalid for text objects")
}


`[[<-.corpus_text` <- function(x, i, value)
{
    stop("[[<- operator is invalid for text objects")
}


format.corpus_text <- function(x, nchar_max = 60, suffix = "\u2026", ...)
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
        long <- !is.na(len) & (len >= nchar_max + 1)
        str[long] <- paste0(substr(str[long], 1, nchar_max), suffix)
    }
    format(str, justify = "left")
}


print.corpus_text <- function(x, print_max = 6L, ...)
{
    if (length(x) == 0) {
        cat("text(0)\n")
        return(invisible(x))
    }

    if (is.null(print_max) || is.na(print_max) || length(x) <= print_max) {
        xsub <- x
        nextra <- 0
    } else {
        xsub <- x[seq_len(print_max)]
        nextra <- length(x) - print_max
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
        cat("(", length(x), " rows total)\n", sep="")
    }

    invisible(x)
}


summary.corpus_text <- function(object, ...)
{
    value <- c(Length = length(object), Class = "text", Mode = "character")
    class(value) <- c("summaryDefault", "table")
    value
}

as.character.corpus_text <- function(x, ...)
{
    .Call(C_as_character_text, x)
}

as.complex.corpus_text <- function(x, ...)
{
    as.complex(as.character(x, ...))
}

as.data.frame.corpus_text <- function(x, row.names = NULL, optional = FALSE, ...)
{
    nm <- deparse(substitute(x), width.cutoff = 500L)

    nrows <- length(x)

    # get row names
    if (!is.null(row.names)) {
        if (!(is.character(row.names) && length(row.names) == nrows)) {
            stop("'row.names' is not a character vector of length %d", nrows)
        }
    } else if (is.null(row.names)) {
        if (nrows == 0L) {
            row.names <- character()
        } else {
            row.names <- names(x)

            if (is.null(row.names)) {
                row.names <- .set_row_names(nrows)
            }
        }
    }

    if (!is.null(names(x))) {
        names(x) <- NULL
    }

    value <- list(x)
    if (!optional)  {
        names(value) <- nm
    }

    structure(value, row.names = row.names, class = "data.frame")
}

as.double.corpus_text <- function(x, ...)
{
    as.double(as.character(x, ...))
}

as.integer.corpus_text <- function(x, ...)
{
    as.integer(as.character(x, ...))
}

as.logical.corpus_text <- function(x, ...)
{
    as.logical(as.character(x, ...))
}

as.Date.corpus_text <- function(x, format, ...)
{
    as.Date(as.character(x), format, ...)
}

is.character.corpus_text <- function(x)
{
    FALSE
}

all.equal.corpus_text <- function(target, current, ...)
{
    if (!is_text(current)) {
        return(c(paste("Modes: text,", mode(current)),
                 paste("target is text, current is", mode(current))))
    }

    nt <- names(target)
    at <- attributes(target)
    target <- as.character(target)
    names(target) <- nt

    for (a in names(at)) {
        if (!(a %in% c("names", "class"))) {
            attr(target, a) <- at[[a]]
        }
    }

    nc <- names(current)
    ac <- attributes(current)
    current <- as.character(current)
    names(current) <- nc

    for (a in names(ac)) {
        if (!(a %in% c("names", "class"))) {
            attr(current, a) <- ac[[a]]
        }
    }

    all.equal(target, current, ...)
}

is.na.corpus_text <- function(x)
{
    .Call(C_is_na_text, x)
}

anyNA.corpus_text <- function(x, recursive = FALSE)
{
    .Call(C_anyNA_text, x)
}

Ops.corpus_text <- function(e1, e2)
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

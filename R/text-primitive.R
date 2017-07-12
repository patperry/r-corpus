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


`[.corpus_text` <- function(x, i)
{
    if (!is_text(x)) {
        stop("invalid text object")
    }

    with_rethrow({
        index <- seq_along(x)
        names(index) <- names(x)
        i <- index[i]
    })

    y <- unclass(x)
    y$handle <- .Call(C_subset_text_handle, y$handle, as.double(i))
    y$table <- y$table[i,]
    y$names <- y$names[i]
    class(y) <- class(x)
    y
}


`[<-.corpus_text` <- function(x, i, value)
{
    stop("'[<-' is not implemented for text objects yet;",
         " to request this feature, please file an issue at",
         " https://github.com/patperry/r-corpus/issues")
}


`[[.corpus_text` <- function(x, i, exact = TRUE)
{
    with_rethrow({
        index <- seq_along(x)
        names(index) <- names(x)
        i <- index[[i, exact = exact]]
    })
    as.character(x[i])
}


`[[<-.corpus_text` <- function(x, i, value)
{
    stop("'[[<-' is not implemented for text objects yet;",
         " to request this feature, please file an issue at",
         " https://github.com/patperry/r-corpus/issues")
}


`$.corpus_text` <- function(x, name)
{
    stop("$ operator is invalid for text objects")
}


`$<-.corpus_text` <- function(x, name, value)
{
    stop("$<- operator is invalid for text objects")
}


anyNA.corpus_text <- function(x, recursive = FALSE)
{
    .Call(C_anyNA_text, x)
}

as.character.corpus_text <- function(x, ...)
{
    .Call(C_as_character_text, x)
}

as.complex.corpus_text <- function(x, ...)
{
    as.complex(as.character(x), ...)
}

as.double.corpus_text <- function(x, ...)
{
    as.double(as.character(x), ...)
}

as.environment.corpus_text <- function(x, ...)
{
    stop("'as.environment' is invalid for text objects")
}

as.integer.corpus_text <- function(x, ...)
{
    as.integer(as.character(x), ...)
}

as.logical.corpus_text <- function(x, ...)
{
    as.logical(as.character(x), ...)
}

as.numeric.corpus_text <- function(x, ...)
{
    as.numeric(as.character(x), ...)
}

as.raw.corpus_text <- function(x, ...)
{
    as.raw(as.character(x), ...)
}

c.corpus_text <- function(..., use.names = TRUE)
{
    args <- list(...)
    for (i in seq_along(args)) {
        elt <- args[[i]]
        if (!is.character(elt)) {
            args[[i]] <- structure(as.character(elt), names = names(elt))
        }
    }
    ans <- c(args, recursive = TRUE, use.names = TRUE)
    as_text(ans)
}

dim.corpus_text <-
dimnames.corpus_text <-
function(x)
{
    NULL
}

`dim<-.corpus_text` <- function(x, value)
{
    stop("dim<- is invalid for text objects")
}

`dimnames<-.corpus_text` <- function(x, value)
{
    stop("dimnames<- is invalid for text objects")
}

is.array.corpus_text <-
is.matrix.corpus_text <-
is.numeric.corpus_text <-
function(x)
{
    FALSE
}

is.finite.corpus_text <-
is.infinite.corpus_text <-
is.nan.corpus_text <-
function(x)
{
    ans <- vector(length = length(x))
    names(ans) <- names(x)
    ans
}

is.na.corpus_text <- function(x)
{
    .Call(C_is_na_text, x)
}

length.corpus_text <- function(x)
{
    .Call(C_length_text, x)
}

`length<-.corpus_text` <- function(x, value)
{
    stop("length<- is invalid for text objects")
}

`levels<-.corpus_text` <- function(x, value)
{
    stop("levels<- is invalid for text objects")
}

names.corpus_text <- function(x)
{
    if (!is_text(x)) {
        stop("invalid text object")
    }
    unclass(x)$names
}

`names<-.corpus_text` <- function(x, value)
{
    if (!is_text(x)) {
        stop("invalid text object")
    }

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

rep.corpus_text <- function(x, ...)
{
    x <- structure(as.character(x), names = names(x))
    as_text(rep(x, ...))
}

xtfrm.corpus_text <- function(x)
{
    xtfrm(as.character(x))
}

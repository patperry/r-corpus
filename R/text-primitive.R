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
    if (!is_corpus_text(x)) {
        stop("invalid text object")
    }

    with_rethrow({
        index <- structure(seq_along(x), names = names(x))
        i <- index[i]
    })

    y <- unclass(x)
    y$handle <- .Call(C_alloc_text_handle)
    y$table <- y$table[i,]

    # drop unused sources
    nsrc <- length(y$sources)
    active <- sort(unique(y$table$source, nmax = nsrc))
    if (length(active) < nsrc) {
        map <- rep(NA_integer_, nsrc)
        map[active] <- seq_along(active)
        y$sources <- y$sources[active]
        y$table$source <- map[y$table$source]
    }

    if (!is.null(y$names)) {
        y$names <- make.unique(y$names[i])
    }
    class(y) <- class(x)
    y
}


`[<-.corpus_text` <- function(x, i, value)
{
    if (!is_corpus_text(x)) {
        stop("invalid text object")
    }

    if (!is_corpus_text(value)) {
        with_rethrow({
            value <- as_character_vector("value", value)
        })
        value <- as_corpus_text(value)
    }

    n <- length(x)
    oldnames <- names(x)
    if (is.null(names(x)) && !missing(i) && is.character(i)) {
        oldnames <- as.character(seq_len(n))
    }
    index <- structure(seq_len(n), names = oldnames)
    with_rethrow({
        index[i] <- n + seq_along(value)
    })
    index[is.na(index)] <- n + length(value) + 1
    if (!is.null(names(index))) {
        empty <- nchar(names(index)) == 0
        names(index)[empty] <- as.character(seq_along(index)[empty])
    }

    y <- c(x, value, NA)[index]
    names(y) <- names(index)
    filter <- unclass(x)$filter
    if (!is.null(filter)) {
        text_filter(y) <- filter
    }

    # copy over old attributes
    attrs <- attributes(x)
    for (k in names(attrs)) {
        if (k != "names") {
            attr(y, k) <- attrs[[k]]
        }
    }

    y
}


`[[.corpus_text` <- function(x, i, exact = TRUE)
{
    if (!is_corpus_text(x)) {
        stop("invalid text object")
    }

    with_rethrow({
        index <- seq_along(x)
        names(index) <- names(x)
        i <- index[[i, exact = exact]]
    })
    as.character(x[i])
}


`[[<-.corpus_text` <- function(x, i, value)
{
    if (!is_corpus_text(x)) {
        stop("invalid text object")
    }

    if (length(i) == 0) {
        stop("attempt to select less than one element")
    } else if (length(i) > 1) {
        stop("attempt to select more than one element")
    }

    if (length(value) == 0) {
        stop("replacement has length zero")
    } else if (length(value) > 1) {
        stop("more elements supplied than there are to replace")
    }

    x[i] <- value
    x
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
    with_rethrow({
        as.complex(as.character(x), ...)
    })
}

as.double.corpus_text <- function(x, ...)
{
    x <- as.character(x)
    with_rethrow({
        as.double(x, ...)
    })
}

as.environment.corpus_text <- function(x, ...)
{
    stop("'as.environment' is invalid for text objects")
}

as.integer.corpus_text <- function(x, ...)
{
    x <- as.character(x)
    with_rethrow({
        as.integer(x, ...)
    })
}

as.logical.corpus_text <- function(x, ...)
{
    x <- as.character(x)
    with_rethrow({
        as.logical(x, ...)
    })
}

as.raw.corpus_text <- function(x, ...)
{
    x <- as.character(x)
    with_rethrow({
        as.raw(x, ...)
    })
}

c.corpus_text <- function(..., recursive = FALSE, use.names = TRUE)
{
    args <- list(...)
    with_rethrow({
        ans <- c_corpus_text_list(args, recursive, use.names)
    })
    ans
}

c_corpus_text_list <- function(args, recursive = FALSE, use.names = TRUE)
{
    filter <- NULL

    # handle recursive part, turning arguments into list of text vectors
    for (i in seq_along(args)) {
        elt <- args[[i]]
        if (is_corpus_text(elt)) {
            # pass
        } else if (recursive && (is.list(elt) || is.pairlist(elt))) {
            elt <- structure(as.list(elt), names = names(elt))
            args[[i]] <- c_corpus_text_list(elt, recursive, use.names)
        } else {
            args[[i]] <- as_corpus_text(elt)
        }

        # take text filter from first arg
        if (i == 1) {
            filter <- unclass(args[[i]])$filter
        }
    }

    # compute the names
    if (use.names) {
        argnames <- names(args)
        ansnames <- vector("list", length(args))
        off <- 0

        for (i in seq_along(args)) {
            name <- argnames[[i]]
            elt <- args[[i]]
            len <- length(elt)

            eltnames <- unclass(elt)$names
            if (length(elt) == 0) {
                # pass
            } else if (is.null(name) || nchar(name) == 0) {
                if (!is.null(eltnames)) {
                    ansnames[[i]] <- eltnames
                }
            } else if (is.null(eltnames)) {
                if (len == 1) {
                    ansnames[[i]] <- name
                } else {
                    ansnames[[i]] <- paste0(name, seq_along(elt))
                }
            } else {
                # ""   -> index (1, 2, 3, etc.)
                # str  -> .str
                # <NA> -> .NA
                suffix <- ifelse(nchar(eltnames) == 0, seq_along(eltnames),
                                 paste0(".", eltnames))
                suffix[is.na(eltnames)] <- ".NA"
                ansnames[[i]] <- paste0(name, suffix)
            }

            off <- off + len
        }

        # if all names are NULL, don't set the names
        if (all(vapply(ansnames, is.null, FALSE))) {
            names <- NULL

        # otherwise, set missing names to empty index, and concatenate
        } else {
            off <- 0
            for (i in seq_along(args)) {
                n <- length(args[[i]])
                if (is.null(ansnames[[i]])) {
                    ansnames[[i]] <- as.character(off + seq_len(n))
                }
                off <- off + n
            }
            names <- c(ansnames, recursive = TRUE)

            if (anyDuplicated(names)) {
                names <- make.unique(names)
            }
        }
    } else {
        names <- NULL
    }

    # concatenate the text vectors and set names, if desired
    .Call(C_text_c, args, names, filter)
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
    if (!is_corpus_text(x)) {
        stop("invalid text object")
    }

    with_rethrow({
        n <- as_length("length", value)
    })

    n0 <- length(x)
    if (n0 > n) {
        x <- x[seq_len(n)]
    } else if (n0 < n) {
        x <- c(x, rep(NA_character_, n - n0))
    }

    x
}

`levels<-.corpus_text` <- function(x, value)
{
    stop("levels<- is invalid for text objects")
}

names.corpus_text <- function(x)
{
    if (!is_corpus_text(x)) {
        stop("invalid text object")
    }
    unclass(x)$names
}

`names<-.corpus_text` <- function(x, value)
{
    if (!is_corpus_text(x)) {
        stop("invalid text object")
    }

    if (!is.null(value)) {
        value <- as.character(value)
        if (length(value) != length(x)) {
            stop(paste0("names attribute [", length(value), "]",
                        " must be the same length as the text object [",
                        length(x), "]"))
        }
        if (anyNA(value)) {
            stop("missing values in 'names' are not allowed")
        }
        if (anyDuplicated(value)) {
            stop("duplicate 'names' are not allowed")
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
    y <- rep(x, ...)
    if (!is.null(names(y))) {
        names(y) <- make.unique(names(y))
    }
    as_corpus_text(y, filter = text_filter(x))
}

xtfrm.corpus_text <- function(x)
{
    xtfrm(as.character(x))
}

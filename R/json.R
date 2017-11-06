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


read_ndjson <- function(file, mmap = FALSE, simplify = TRUE, text = NULL)
{
    with_rethrow({
        mmap <- as_option("mmap", mmap)
        simplify <- as_option("simplify", simplify)
        text <- as_character_vector("text", text)
    })

    if (mmap) {
        if (!is.character(file)) {
            stop("'file' must be a character string when 'mmap' is TRUE")
        }

        ans <- .Call(C_mmap_ndjson, file, text)

    } else {
        # open the file in binary mode
        if (is.character(file)) {
            file <- file(file, "rb")
            on.exit(close(file))
        }
        if (!inherits(file, "connection")) {
            stop("'file' must be a character string or connection")
        }
        if (!isOpen(file, "rb")) {
            open(file, "rb")
            on.exit(close(file))
        }

        # read the raw data
        size <- 32 * 1024 * 1024 # 32 MB chunks
        buffer <- raw()

        repeat {
            chunk <- readBin(file, raw(), size)
            if (length(chunk) == 0) {
                break
            }
            buffer <- c(buffer, chunk)
            size <- min(.Machine$integer.max, 2 * size)
        }

        ans <- .Call(C_read_ndjson, buffer, text)
    }

    if (simplify) {
        if (length(dim(ans)) == 2) {
            ans <- as.data.frame(ans, text = text)
        } else {
            ans <- .Call(C_simplify_json, ans)
        }
    }
    ans
}


dim.corpus_json <- function(x)
{
    .Call(C_dim_json, x)
}


length.corpus_json <- function(x)
{
    .Call(C_length_json, x)
}


names.corpus_json <- function(x)
{
    .Call(C_names_json, x)
}


`names<-.corpus_json` <- function(x, value)
{
    stop("setting names on a JSON object is not allowed")
}


dimnames.corpus_json <- function(x)
{
    if (is.null(dim(x))) {
        return(NULL)
    } else {
        rn <- as.character(seq_len(nrow(x)))
        cn <- names(x)
        list(rn, cn)
    }
}


print.corpus_json <- function(x, ...)
{
    invisible(.Call(C_print_json, x))
}


`$.corpus_json` <- function(x, name)
{
    if (is.null(dim(x))) {
        stop("$ operator is invalid for scalar JSON objects")
    }
    x[[name]]
}


`$<-.corpus_json` <- function(x, name, value)
{
    stop("$<- operator is invalid for JSON objects")
}


`[[.corpus_json` <- function(x, i)
{
    if (length(i) == 0) {
        stop("subscript of length 0 is not allowed")
    } else if (length(i) > 1) {
        stop("subscript of length >1 is not allowed")
    }
    if (is.character(i)) {
        with_rethrow({
            i <- as_utf8(i)
        })
    }

    # For scalar json objects, i is the index.
    #
    # For record objects, i can either be the column index, or
    # the column name; in the latter case convert i to the column
    # index.
    #
    # In both cases, convert i to a whole number double.

    if (is.null(dim(x))) { # scalar json
        if (!is.numeric(i) || is.na(i)) {
            stop(paste0("invalid subscript: \"", i, "\""))
        }
        i <- floor(as.double(i))
    } else { # record json
        if (is.character(i)) {
            if (!(i %in% names(x))) {
                stop(paste0("invalid column name: \"", i, "\""))
            }
            i <- match(i, names(x))
        }
        if (!is.numeric(i) || is.na(i)) {
            stop(paste0("invalid subscript: \"", i, "\""))
        }
        i <- floor(as.double(i))
    }

    ans <- .Call(C_subscript_json, x, i)
    ans <- .Call(C_simplify_json, ans)
    ans
}


`[[<-.corpus_json` <- function(x, i, value)
{
    stop("[[<- operator is invalid for JSON objects")
}



`[.corpus_json` <- function(x, i, j, drop = TRUE)
{
    if (missing(i)) {
        i <- NULL
    } else if (is.null(i)) {
        i <- numeric()
    } else if (is.character(i)) {
        i <- as_utf8(i)
    }

    if (missing(j)) {
        ni <- 1
        j <- NULL
    } else if (is.null(dim(x))) {
        stop("incorrect number of dimensions")
    } else {
        ni <- 2
        if (is.null(j)) {
            j <- integer()
        } else if (is.character(j)) {
            j <- as_utf8(j)
        }
    }

    with_rethrow({
        drop <- as_option("drop", drop)
    })

    if (!is.null(i)) {
        ix <- seq_len(NROW(x))
        if (!is.null(dim(x))) {
            names(ix) <- row.names(x)
        }
        i <- ix[i]
        i <- as.double(i)
    }

    if (!is.null(j)) {
        if (length(j) == 0) {
            stop("second subscript of length 0 is not allowed")
        } else if (length(j) > 1) {
            stop("second subscript of length >1 is not allowed")
        }

        if (is.character(j)) {
            if (!j %in% names(x)) {
                stop(paste0("invalid column name: \"", j, "\""))
            }
            j <- match(j, names(x))
        }

        if (!is.numeric(j) || is.na(j)) {
            stop(paste0("invalid column subscript: \"", j, "\""))
        }
        j <- floor(as.double(j))
    }

    ans <- .Call(C_subset_json, x, i, j)
    if (drop && !is.null(dim(ans)) && length(ans) == 1) {
        ans <- .Call(C_simplify_json, ans)
    }
    ans
}


`[<-.corpus_json` <- function(x, ..., value)
{
    stop("[<- operator is invalid for JSON objects")
}


as.character.corpus_json <- function(x, ...)
{
    .Call(C_as_character_json, x)
}


as.data.frame.corpus_json <- function(x, row.names = NULL, ...,
                                      text = NULL, stringsAsFactors = FALSE)
{
    with_rethrow({
        text <- as_character_vector("text", text)
        stringsAsFactors <- as_option("stringsAsFactors", stringsAsFactors)
    })

    if (is.null(dim(x))) {
        n <- length(x)
        l <- list(.Call(C_simplify_json, x))
        names(l) <- deparse(substitute(x), width.cutoff = 500L)
    } else {
        n <- nrow(x)
        l <- as.list.corpus_json(x, text = text)
    }

    if (!is.null(row.names)) {
        with_rethrow({
            row.names <- as_names("row.names", row.names, n)
        })
    } else {
        row.names <- c(NA, -n)
    }

    cols <- list()
    names <- character()
    ncol <- 0

    for (i in seq_along(l)) {
        elt <- l[[i]]
        nm <- names(l)[[i]]

        if (inherits(elt, "corpus_json")) {
            nested <- as.data.frame(elt, ..., text = text,
                                    stringsAsFactors = stringsAsFactors)
            for (j in seq_along(nested)) {
                ncol <- ncol + 1L
                cols[[ncol]] <- nested[[j]]
                names[[ncol]] <- paste(nm, names(nested)[[j]], sep = ".")
            }
        } else {
            if (is.character(elt) && stringsAsFactors) {
                elt <- as.factor(elt)
            }
            ncol <- ncol + 1L
            cols[[ncol]] <- elt
            names[[ncol]] <- nm
        }
    }

    names(cols) <- names
    structure(cols, row.names = row.names,
              class = c("corpus_frame", "data.frame"))
}


as.logical.corpus_json <- function(x, ...)
{
    .Call(C_as_logical_json, x)
}


as.integer.corpus_json <- function(x, ...)
{
    .Call(C_as_integer_json, x)
}


as.double.corpus_json <- function(x, ...)
{
    .Call(C_as_double_json, x)
}


as.list.corpus_json <- function(x, ...)
{
    .Call(C_as_list_json, x)
}

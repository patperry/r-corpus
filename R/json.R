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

        ans <- .Call(C_mmap_ndjson, file)

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
        }

        ans <- .Call(C_read_ndjson, buffer)
    }

    if (simplify) {
        if (length(dim(ans)) == 2) {
            ans <- as.data.frame(ans, text = text)
        } else {
            ans <- .Call(C_simplify_json, ans, text)
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
    cn <- names(x)
    if (is.null(cn)) {
        NULL
    } else {
        list(NULL, cn)
    }
}


json_datatype <- function(x)
{
    .Call(C_json_datatype, x)
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
    ans <- .Call(C_simplify_json, ans, NULL)
    ans
}


`[[<-.corpus_json` <- function(x, i, value)
{
    stop("[[<- operator is invalid for JSON objects")
}



`[.corpus_json` <- function(x, ...)
{
    if (!all(names(sys.call()[-1] == ""))) {
        stop("named arguments are not allowed")
    }

    ni <- nargs() - 1

    if (is.null(dim(x))) {
        # Scalar json:
        # If non-NULL, convert i to a double() vector of indices;
        # set j to NULL.

        if (ni > 1) {
            stop("incorrect number of dimensions")
	    }

        if (ni < 1 || missing(..1)) {
	        i <- NULL
	    } else {
            i <- seq_len(NROW(x))[..1]
            i <- as.double(i)
        }
	    j <- NULL
    } else {
        # Record json:
        # If non-NULL, convert i to a double() vector of indices;
        # if non-NULL convert j to a column index

        if (ni > 2) {
            stop("incorrect number of dimensions")
	    }

        if (ni < 1 || missing(..1)) {
            i <- NULL
	    } else {
            i <- seq_len(NROW(x))[..1]
            i <- as.double(i)
        }


        if (ni < 2 || missing(..2)) {
            j <- NULL
        } else {
            j <- ..2
            if (length(j) == 0) {
                stop("second subscript of length 0 is not allowed")
            } else if (length(j) > 1) {
                stop("second subscript of length >1 is not allowed")
            }

            if (is.character(j)) {
                if (!(j %in% names(x))) {
                    stop(paste0("invalid column name: \"", j, "\""))
                }
                j <- match(j, names(x))
            }
            if (!is.numeric(j) || is.na(j)) {
                stop(paste0("invalid column subscript: \"", j, "\""))
            }
            j <- floor(as.double(j))
        }
    }

    .Call(C_subset_json, x, i, j)
}


`[<-.corpus_json` <- function(x, ..., value)
{
    stop("[<- operator is invalid for JSON objects")
}


as.character.corpus_json <- function(x, ...)
{
    .Call(C_as_character_json, x)
}


as.data.frame.corpus_json <- function(x, row.names = NULL, optional = TRUE,
                                      text = NULL, ..., cut.names = FALSE,
                                      col.names = NULL,
                                      fix.empty.names = FALSE,
                                      stringsAsFactors = FALSE)
{
    with_rethrow({
        text <- as_character_vector("text", text)
    })

    if (is.null(col.names)) {
        if (is.null(dim(x))) {
            col.names <- deparse(substitute(x), width.cutoff = 500L)
        } else {
            col.names <- names(x)
        }
    }

    if (is.null(dim(x))) {
        l <- list(.Call(C_simplify_json, x, text))
    } else {
        l <- as.list(x, text = text)
    }

    structure(as.data.frame(l, row.names = row.names, optional = optional,
                            ..., cut.names = cut.names, col.names = col.names,
                            fix.empty.names = fix.empty.names,
                            stringsAsFactors = stringsAsFactors),
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


as.list.corpus_json <- function(x, text = NULL, ...)
{
    with_rethrow({
        text <- as_character_vector("text", text)
    })
    .Call(C_as_list_json, x, text)
}


as_text.corpus_json <- function(x, ...)
{
    if (length(dim(x)) == 2) {
        if (!("text" %in% names(x))) {
            stop("no column named 'text'")
        }
        nm <- row.names(x)
        x <- as_text(x$text)
        names(x) <- nm
        x
    } else {
        .Call(C_as_text_json, x)
    }
}

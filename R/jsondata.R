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


read_ndjson <- function(file, mmap = FALSE, simplify = TRUE, text = "text",
                        stringsAsFactors = default.stringsAsFactors())
{
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
        text <- as.character(text)
        if (length(dim(ans)) == 2) {
            ans <- as.data.frame(ans, text = text,
                                 stringsAsFactors = stringsAsFactors)
        } else {
            ans <- .Call(C_simplify_jsondata, ans, text, stringsAsFactors)
        }
    }
    ans
}


dim.jsondata <- function(x)
{
    .Call(C_dim_jsondata, x)
}


length.jsondata <- function(x)
{
    .Call(C_length_jsondata, x)
}


names.jsondata <- function(x)
{
    .Call(C_names_jsondata, x)
}


`names<-.jsondata` <- function(x, value)
{
    stop("setting names on a jsondata object is not allowed")
}


dimnames.jsondata <- function(x)
{
    cn <- names(x)
    if (is.null(cn)) {
        NULL
    } else {
        list(NULL, cn)
    }
}


datatype.jsondata <- function(x, ...)
{
    .Call(C_datatype_jsondata, x)
}


datatypes.jsondata <- function(x, ...)
{
    .Call(C_datatypes_jsondata, x)
}


print.jsondata <- function(x, ...)
{
    invisible(.Call(C_print_jsondata, x))
}


`$.jsondata` <- function(x, name)
{
    if (is.null(dim(x))) {
        stop("$ operator is invalid for scalar jsondatas")
    }
    x[[name]]
}


`$<-.jsondata` <- function(x, name, value)
{
    stop("$<- operator is invalid for jsondata objects")
}


`[[.jsondata` <- function(x, i)
{
    if (length(i) == 0) {
        stop("subscript of length 0 is not allowed")
    } else if (length(i) > 1) {
        stop("subscript of length >1 is not allowed")
    }

    # For scalar jsondatas, i is the index.
    #
    # For record jsondatas, i can either be the column index, or
    # the column name; in the latter case convert i to the column
    # index.
    #
    # In both cases, convert i to a whole number double.

    if (is.null(dim(x))) { # scalar jsondata
        if (!is.numeric(i) || is.na(i)) {
            stop(paste0("invalid subscript: \"", i, "\""))
        }
        i <- floor(as.double(i))
    } else { # record jsondata
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

    ans <- .Call(C_subscript_jsondata, x, i)
    ans <- .Call(C_simplify_jsondata, ans)
    ans
}


`[[<-.jsondata` <- function(x, i, value)
{
    stop("[[<- operator is invalid for jsondata objects")
}



`[.jsondata` <- function(x, ...)
{
    if (!all(names(sys.call()[-1] == ""))) {
        stop("named arguments are not allowed")
    }

    ni <- nargs() - 1

    if (is.null(dim(x))) {
        # Scalar jsondata:
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
        # Record jsondata:
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

    .Call(C_subset_jsondata, x, i, j)
}


`[<-.jsondata` <- function(x, ..., value)
{
    stop("[<- operator is invalid for jsondata objects")
}


as.character.jsondata <- function(x, ...)
{
    as.character(C_as_character_jsondata, x)
}


as.data.frame.jsondata <-
    function(x, ..., text = "text",
             stringsAsFactors = default.stringsAsFactors())
{
    l <- as.list(x, text = text, stringsAsFactors = stringsAsFactors)
    as.data.frame(l, row.names = row.names(x), ...,
                  text = text, stringsAsFactors = stringsAsFactors)
}


as.logical.jsondata <- function(x, ...)
{
    .Call(C_as_logical_jsondata, x)
}


as.integer.jsondata <- function(x, ...)
{
    .Call(C_as_integer_jsondata, x)
}


as.double.jsondata <- function(x, ...)
{
    .Call(C_as_double_jsondata, x)
}


as.list.jsondata <- function(x, ..., text = "text",
                             stringsAsFactors = default.stringsAsFactors())
{
    text <- as.character(text)
    .Call(C_as_list_jsondata, x, text, stringsAsFactors)
}


as_text.jsondata <- function(x, ...)
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
        .Call(C_as_text_jsondata, x)
    }
}

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

dim.dataset <- function(x)
{
    .Call(C_dim_dataset, x)
}


length.dataset <- function(x)
{
    .Call(C_length_dataset, x)
}


names.dataset <- function(x)
{
    .Call(C_names_dataset, x)
}


`names<-.dataset` <- function(x, value)
{
    stop("setting names on a dataset object is not allowed")
}


dimnames.dataset <- function(x)
{
    cn <- names(x)
    if (is.null(cn)) {
        NULL
    } else {
        list(NULL, cn)
    }
}


datatype.dataset <- function(x, ...)
{
    .Call(C_datatype_dataset, x)
}


datatypes.dataset <- function(x, ...)
{
    .Call(C_datatypes_dataset, x)
}


print.dataset <- function(x, ...)
{
    invisible(.Call(C_print_dataset, x))
}


`$.dataset` <- function(x, name)
{
    if (is.null(dim(x))) {
        stop("$ operator is invalid for scalar datasets")
    }
    x[[name]]
}


`$<-.dataset` <- function(x, name, value)
{
    stop("$<- operator is invalid for dataset objects")
}


`[[.dataset` <- function(x, i)
{
    if (length(i) == 0) {
        stop("subscript of length 0 is not allowed")
    } else if (length(i) > 1) {
        stop("subscript of length >1 is not allowed")
    }

    if (!is.null(dim(x))) {
        if (is.character(i)) {
            name <- i
            i <- match(name, names(x))
            if (is.na(i)) {
                stop(paste0("invalid column name: \"", name, "\""))

            }
        }
    }

    i <- floor(as.double(i))
    ans <- .Call(C_subscript_dataset, x, i)
    ans <- .Call(C_simplify_dataset, ans)
    ans
}


`[[<-.dataset` <- function(x, i, value)
{
    stop("[[<- operator is invalid for dataset objects")
}



`[.dataset` <- function(x, ...)
{
    if (!all(names(sys.call()[-1] == ""))) {
        stop("named arguments are not allowed")
    }

    ni <- nargs() - 1

    if (is.null(dim(x))) {
        # scalar dataset

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
        # record dataset

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
                name <- j
                j <- match(name, names(x))
                if (is.na(j)) {
                    stop(paste0("invalid column name: \"", name, "\""))
                }
            }

            j <- as.double(j)
        }
    }

    .Call(C_subset_dataset, x, i, j)
}


`[<-.dataset` <- function(x, ..., value)
{
    stop("[<- operator is invalid for dataset objects")
}


as.character.dataset <- function(x, ...)
{
    as.character(as.text.dataset(x, ...))
}


as.logical.dataset <- function(x, ...)
{
    .Call(C_as_logical_dataset, x)
}


as.integer.dataset <- function(x, ...)
{
    .Call(C_as_integer_dataset, x)
}


as.double.dataset <- function(x, ...)
{
    .Call(C_as_double_dataset, x)
}


as.list.dataset <- function(x, ...)
{
    .Call(C_as_list_dataset, x)
}


as.text.dataset <- function(x, ...)
{
    .Call(C_as_text_dataset, x)
}

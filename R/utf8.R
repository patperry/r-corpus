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


# converts a character vector from its declared encoding to UTF-8
as_utf8 <- function(x)
{
    .Call(C_utf8_coerce, x)
}

# encode an R character string in a form suitable for display
# in the current locale (determined by LC_CTYPE)
utf8_encode <- function(x, display = FALSE)
{
    utf8 <- (Sys.getlocale("LC_CTYPE") != "C")
    .Call(C_utf8_encode, x, display, utf8)
}


utf8_format <- function(x, trim = FALSE, chars = NULL, justify = "left",
                        width = NULL, na.encode = TRUE, quote = FALSE,
                        na.print = NULL, ...)
{
    if (is.null(x)) {
        return(NULL)
    }

    if (!is.character(x)) {
        stop("argument is not a character object")
    }

    with_rethrow({
        trim <- as_option("trim", trim)
        chars <- as_chars("chars", chars)
        justify <- as_justify("justify", justify)
        width <- as_integer_scalar("width", width)
        na.encode <- as_option("na.encode", na.encode)
        quote <- as_option("quote", quote)
        na.print <- as_na_print("na.print", na.print)
    })

    utf8 <- Sys.getlocale("LC_CTYPE") != "C"
    .Call(C_utf8_format, x, trim, chars, justify, width, na.encode,
          quote, na.print, utf8)
}


utf8_print <- function(x, chars = NULL, quote = TRUE, na.print = NULL,
                       print.gap = NULL, right = FALSE, max = NULL,
                       display = TRUE, ...)
{
    if (is.null(x)) {
        return(invisible(NULL))
    }

    if (!is.character(x)) {
        stop("argument is not a character object")
    }

    with_rethrow({
        chars <- as_chars("chars", chars)
        quote <- as_option("quote", quote)
        na.print <- as_na_print("na.print", na.print)
        print.gap <- as_print_gap("print_gap", print.gap)
        right <- as_option("right", right)
        max <- as_max_print("max", max)
        display <- as_option("display", display)
    })

    if (is.null(print.gap)) {
        print.gap <- 1L
    }
    if (is.null(max)) {
        max <- getOption("max.print")
    }
    width <- getOption("width")
    
    n <- length(x)
    dim <- dim(x)
    vec <- is.null(dim(x)) || length(dim(x)) == 1
    justify <- if (right) "right" else "left"

    fmt <- utf8_format(x, trim = !vec, chars = chars,
                       justify = justify, na.encode = TRUE,
                       quote = quote, na.print = na.print)
    names <- names(fmt)
    dimnames <- dimnames(fmt)

    if (is.null(dim) || length(dim) == 1) {
        if (length(dim) == 1 && !is.null(dimnames)) {
            names <- dimnames[[1]]
        }
    } else {
        if (is.null(dimnames)) {
            dimnames <- vector("list", length(dim))
        }

        if (is.null(dimnames[[1]]) && dim[[1]] > 0) {
            dimnames[[1]] <-
                utf8_format(paste0("[", seq_len(dim[[1]]), ",]"),
                            justify = "right")
        }
        if (is.null(dimnames[[2]]) && dim[[2]] > 0) {
            dimnames[[2]] <- paste0("[,", seq_len(dim[[2]]), "]")
        }
        if (length(dim) > 2) {
            for (i in 2:length(dim)) {
                if (is.null(dimnames[[i]] && dim[[i]] > 0)) {
                    dimnames[[i]] <- as.character(seq_len(dim[[i]]))
                }
            }
        }
    }

    fmt <- utf8_encode(fmt, display)
    if (is.null(dim)) {
        names <- utf8_encode(names, display)
        names(fmt) <- names
    } else {
        dimnames <- lapply(dimnames, utf8_encode, display = display)
        dimnames(fmt) <- dimnames
    }

    if (vec) {
        if (n == 0) {
            cat("character(0)\n")
            return(invisible(x))
        }

        if (is.null(names) && n > 0) {
            labels <- utf8_format(paste0("[", seq_len(n), "]"),
                                  justify = "right")
        } else {
            labels <- names
        }

        width <- getOption("width")
        namewidth <- max(0, utf8_width(labels))
        elt <- max(0, utf8_width(fmt))


        if (!is.null(names)) {
            ncol <- max(1, width %/% (max(namewidth, elt) + print.gap))
            extra <- n %% ncol

            off <- 0
            while (off + ncol <= n) {
                ix <- (off+1):(off+ncol)
                mat <- matrix(fmt[ix], ncol = ncol, byrow = TRUE,
                              dimnames = list(NULL, names[ix]))
                off <- off + ncol
                .Call(C_print_table, mat, print.gap, right, width)
            }

            if (extra > 0) {
                ix <- n - extra + seq_len(extra)
                last <- rbind(as.vector(fmt[ix]))
                rownames(last) <- NULL
                colnames(last) <-  names[ix]
                .Call(C_print_table, last, print.gap, right, width)
            }
        } else {
            ncol <- max(1, (width - namewidth) %/% (elt + print.gap))
            extra <- n %% ncol

            mat <- matrix(fmt[seq_len(n - extra)], ncol = ncol, byrow = TRUE)
            rownames(mat) <- labels[seq(from = 1, by = ncol,
                                        length.out = nrow(mat))]
            .Call(C_print_table, mat, print.gap, right, width)

            if (extra > 0) {
                last <- rbind(as.vector(fmt[n - extra  + seq_len(extra)]))
                rownames(last) <- labels[n - extra + 1]
                .Call(C_print_table, last, print.gap, right, width)
            }
        }
    } else if (length(dim) == 2) {
        if (dim[1] == 0 && dim[2] == 0) {
            cat("<0 x 0 matrix>\n")
        } else {
            .Call(C_print_table, fmt, print.gap, right, width)
        }
    } else {
        # print array
        stop("not implemented")
    }

    invisible(x)
}


# test whether the elements can be converted to valid UTF-8
utf8_valid <- function(x)
{
    .Call(C_utf8_valid, x)
}

# gets the width; NA for invalid or nonprintable sequences
utf8_width <- function(x, encode = TRUE)
{
    with_rethrow({
        encode <- as_option("encode", encode)
    })
    
    if (encode) {
        x <- utf8_encode(x)
    }
    utf8 <- (Sys.getlocale("LC_CTYPE") != "C")
    .Call(C_utf8_width, x, utf8)
}

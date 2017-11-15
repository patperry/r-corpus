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

format.corpus_frame <- function(x, chars = NULL, na.encode = TRUE,
                                quote = FALSE, na.print = NULL,
                                print.gap = NULL, ..., justify = "none")
{
    if (is.null(x)) {
        return(invisible(NULL))
    } else if (!is.data.frame(x)) {
        stop("argument is not a data frame")
    }

    with_rethrow({
        chars <- as_chars("chars", chars)
        na.encode <- as_option("na.encode", na.encode)
        quote <- as_option("quote", quote)
        na.print <- as_na_print("na.print", na.print)
        print.gap <- as_print_gap("print.gap", print.gap)
        justify <- as_justify("justify", justify)
    })

    if (is.null(na.print)) {
        na.print <- if (quote) "NA" else "<NA>"
    }
    if (is.null(print.gap)) {
        print.gap <- 1L
    }

    nr <- .row_names_info(x, 2L)
    nc <- ncol(x)
    names <- names(x)
    names[is.na(names)] <- "<NA>"
    rownames <- rownames(x)
    stopifnot(!anyNA(rownames))

    if ((stretch <- is.null(chars))) {
        width <- getOption("width")
        rw <- max(0L, utf8_width(rownames))

        utf8 <- output_utf8()
        ellipsis <- if (utf8) 1 else 3
        quotes <- if (quote) 2 else 0

        chars_min <- 24
        chars_max <- max(chars_min, width - ellipsis - quotes - print.gap - rw)
        chars <- chars_left <- chars_max
    }

    cols <- vector("list", length(x))

    for (i in seq_len(nc)) {
        if (stretch) {
            if (chars_left < chars_min) {
                # allow wrap to next line
                chars <- chars_max
            } else {
                chars <- chars_left
            }
        }

        elt <- x[[i]]
        cl <- class(elt)

        # determine the name width
        w <- utf8_width(names[[i]])

        # convert factor to character
        if (is.factor(elt) && (identical(cl, "factor")
                               || identical(cl, c("AsIs", "factor")))) {
            elt <- structure(as.character(elt), names = names(elt),
                             dim = dim(elt), dimnames = dimnames(elt))
            cl <- class(elt)
        }

        # format character using utf8_format
        if (is.character(elt) && (identical(cl, "character")
                                  || identical(cl, "AsIs"))) {
            cols[[i]] <- utf8_format(elt, chars = chars, justify = justify,
                                     width = w, na.encode = na.encode,
                                     quote = quote, na.print = na.print)
            char <- TRUE
        } else { # format others using S3
            cols[[i]] <- format(elt, chars = chars, justify = justify,
                                width = w, na.encode = na.encode,
                                quote = quote, na.print = na.print,
                                print.gap = print.gap, ...)
            char <- is_corpus_text(elt)
        }

        # determine column width
        w <- max(w, utf8_width(cols[[i]], quote = quote), na.rm = TRUE)
        if (anyNA(cols[[i]])) {
            naw <- if (char) utf8_width(na.print) else 2 # NA
            w <- max(w, naw)
        }

        # format the name, using element justification
        if (!char) {
            names[[i]] <- utf8_format(names[[i]], chars = .Machine$integer.max,
                                      justify = "right", width = w)
        }

        if (stretch) {
            if (w > chars_left) {
                # wrapped to next line
                chars_left <- chars_max - print.gap - w
            } else {
                chars_left <- chars_left - print.gap - w
            }

            if (chars_left < 0) {
                chars_left <- chars_max
            }
        }
    }

    lens <- vapply(cols, NROW, 1)
    if (any(lens != nr)) {
        stop("invalid data frame: columns have nonuniform lengths")
    }

    for (i in seq_len(nc)) {
        if (is.character(cols[[i]]) && inherits(cols[[i]], "character")) {
            oldClass(cols[[i]]) <- "AsIs"
        }
    }

    # Should we truncate long names? R cuts them off at 256 characters.
    names(cols) <- names

    structure(cols, class = c("corpus_frame", "data.frame"),
              row.names = rownames)

}


print.corpus_frame <- function(x, rows = 20L, chars = NULL, digits = NULL,
                               quote = FALSE, na.print = NULL,
                               print.gap = NULL, right = FALSE,
                               row.names = TRUE, max = NULL,
                               display = TRUE, ...)
{
    if (is.null(x)) {
        return(invisible(NULL))
    } else if (!is.data.frame(x)) {
        stop("argument is not a data frame")
    }

    n <- nrow(x)
    nc <- length(x)

    with_rethrow({
        rows <- as_rows("rows", rows)
        chars <- as_chars("chars", chars)
        digits <- as_digits("digits", digits)
        quote <- as_option("quote", quote)
        na.print <- as_na_print("na.print", na.print)
        print.gap <- as_print_gap("print_gap", print.gap)
        right <- as_option("right", right)
        if (!isTRUE(row.names)) {
            if (identical(row.names, FALSE)) {
                row.names <- rep("", n)
            } else {
                row.names <- as_names("row.names", row.names, n, unique = FALSE)
            }
        }
        max <- as_max_print("max", max)
        display <- as_option("display", display)
    })

    if (is.null(rows) || rows < 0) {
        rows <- .Machine$integer.max
    }

    if (length(x) == 0) {
        cat(sprintf(ngettext(n, "data frame with 0 columns and %d row",
                             "data frame with 0 columns and %d rows"), n),
            "\n", sep = "")
        return(invisible(x))
    } else if (n == 0 && is.null(names(x))) {
        cat(sprintf(ngettext(nc, "data frame with %d column and 0 rows",
                             "data frame with %d columns and 0 rows"), nc),
            "\n", sep = "")
        return(invisible(x))
    }

    trunc <- (!is.null(rows) && n > rows)
    if (trunc) {
        xsub <- x[seq_len(rows), , drop = FALSE]
        if (!isTRUE(row.names)) {
            row.names <- row.names[seq_len(rows)]
        }
    } else {
        xsub <- x
    }

    fmt <- format.corpus_frame(xsub, chars = chars, na.encode = FALSE,
                               na.print = na.print, quote = quote,
                               print.gap = print.gap, digits = digits)
    m <- as.matrix(fmt)
    storage.mode(m) <- "character"

    if (!isTRUE(row.names)) {
        rownames(m) <- row.names
    }

    utf8_print(m, chars = .Machine$integer.max, quote = quote,
               na.print = na.print, print.gap = print.gap,
               right = right, max = max, names = style_bold,
               rownames = style_faint, escapes = style_faint,
               display = display)

    if (n == 0) {
        cat("(0 rows)\n")
    } else if (trunc) {
        name_width <- max(0, utf8_width(rownames(m)))

        ellipsis <- ifelse(output_utf8(), "\u22ee", ".")
        ellipsis <- substr(ellipsis, 1, name_width)
        gap <- if (is.null(print.gap)) 1 else print.gap
        space <- format(ellipsis, width = name_width + gap)
        if (output_ansi()) {
            space <- paste0("\x1b[", style_faint, "m", space, "\x1b[0m")
        }
        cat(space, sprintf("(%d rows total)\n", n), sep = "")
    }

    invisible(x)
}

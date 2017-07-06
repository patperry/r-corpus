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

format.corpus_frame <- function(x, chars = NULL, justify = "left",
                                na.encode = TRUE, quote = FALSE,
                                na.print = NULL, print.gap = NULL, ...)
{
    if (is.null(x)) {
        return(invisible(NULL))
    }

    if (!is.data.frame(x)) {
        stop("argument is not a data frame")
    }

    with_rethrow({
        chars <- as_chars("chars", chars)
        justify <- as_justify("justify", justify)
        na.encode <- as_option("na.encode", na.encode)
        quote <- as_option("quote", quote)
        na.print <- as_na_print("na.print", na.print)
        print.gap <- as_print_gap("print.gap", print.gap)
    })

    if (is.null(chars)) {
        utf8 <- Sys.getlocale("LC_CTYPE") != "C"
        ellipsis <- if (utf8) 1 else 3
        quotes <- if (quote) 2 else 0
        gap <- if (is.null(print.gap)) 1 else print.gap
        width <- getOption("width")
        chars <- (width - ellipsis - quotes
                  - gap - max(0, utf8_width(rownames(x))))
        chars <- max(chars, 12)
    }

    nr <- .row_names_info(x, 2L)
    nc <- ncol(x)
    names <- names(x)

    cols <- vector("list", nc)
    for (i in seq_len(nc)) {
        elt <- x[[i]]
        cl <- class(elt)
        fac <- FALSE

        if (is.factor(elt) && (identical(cl, "factor")
                               || identical(cl, c("AsIs", "factor")))) {
            elt <- structure(as.character(elt), names = names(elt),
                             dim = dim(elt), dimnames = dimnames(elt))
            cl <- class(elt)
        }

        if (is.character(elt) && (identical(cl, "character")
                                  || identical(cl, "AsIs"))) {
            w <- if (is.na(names[[i]])) 2 else utf8_width(names[[i]])
            cols[[i]] <- utf8_format(elt, chars = chars, justify = justify,
                                     width = w, na.encode = na.encode,
                                     quote = quote, na.print = na.print)
            char <- TRUE

        } else {
            cols[[i]] <- format(elt, chars = chars, justify = justify,
                                na.encode = na.encode, quote = quote,
                                na.print = na.print,
                                print.gap = print.gap, ...)
            char <- FALSE
        }

        if (char || is_text(elt)) {
            # use same justification for column and name
            names[[i]] <- utf8_format(names[[i]],
                                      chars = .Machine$integer.max,
                                      justify = justify,
                                      width = max(0, utf8_width(cols[[i]])),
                                      na.encode = TRUE, na.print = "NA")
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
    row.names <- row.names(x)

    structure(cols, class = c("corpus_frame", "data.frame"),
              row.names = row.names)
}


print.corpus_frame <- function(x, rows = 20L, chars = NULL, digits = NULL,
                               quote = FALSE, na.print = NULL,
                               print.gap = NULL, right = FALSE,
                               row.names = TRUE, max = NULL,
                               display = TRUE, ...)
{
    if (is.null(x)) {
        return(invisible(NULL))
    }

    if (!is.data.frame(x)) {
        stop("argument is not a data frame")
    }

    n <- nrow(x)
    nc <- length(x)

    with_rethrow({
        rows <- as_max_print("rows", rows)
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
                row.names <- as_names("row.names", row.names, n)
            }
        }
        max <- as_max_print("max", max)
        display <- as_option("display", display)
    })

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
    } else {
        xsub <- x
    }

    justify <- if (right) "right" else "left"

    fmt <- format.corpus_frame(xsub, chars = chars, digits = digits,
                               na.encode = TRUE, quote = quote,
                               na.print = na.print, justify = justify)
    m <- as.matrix(fmt)
    storage.mode(m) <- "character"

    if (!isTRUE(row.names)) {
        rownames(m) <- row.names
    }

    utf8_print(m, chars = .Machine$integer.max, quote = FALSE,
               print.gap = print.gap, right = TRUE, max = max,
               display = display)

    if (n == 0) {
        cat("(0 rows)\n")
    } else if (trunc) {
        ellipsis <- ifelse(Sys.getlocale("LC_CTYPE") == "C", "...", "\u22ee")
        cat(sprintf("%s\n(%d rows total)\n", ellipsis, n))
    }

    invisible(x)
}

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

Complex.corpus_text <- function(z)
{
    stop(sprintf("'%s' is not defined for text objects", .Generic))
}

Math.corpus_text <- function(x, ...)
{
    stop(sprintf("'%s' is not defined for text objects", .Generic))
}

Summary.corpus_text <- function(..., na.rm = FALSE)
{
    stop(sprintf("'%s' is not defined for text objects", .Generic))
}

Ops.corpus_text <- function(e1, e2)
{
    if (nargs() == 1)
        stop(gettextf("unary %s not defined for text objects",
            .Generic), domain = NA)
    boolean <- switch(.Generic, `<` = , `>` = , `==` = , `!=` = ,
        `<=` = , `>=` = TRUE, FALSE)
    if (!boolean)
        stop(gettextf("%s not defined for text objects",
            .Generic), domain = NA)
    e1 <- as.character(e1)
    e2 <- as.character(e2)
    NextMethod(.Generic)
}


all.equal.corpus_text <- function(target, current, ...)
{
    if (!is_text(current)) {
        return(c(paste("Modes: text,", mode(current)),
                 paste("target is text, current is", mode(current))))
    }

    nt <- names(target)
    ft <- text_filter(target)
    at <- attributes(target)
    target <- as.character(target)
    names(target) <- nt

    for (a in names(at)) {
        if (!(a %in% c("names", "class"))) {
            attr(target, a) <- at[[a]]
        }
    }

    nc <- names(current)
    fc <- text_filter(current)
    ac <- attributes(current)
    current <- as.character(current)
    names(current) <- nc

    for (a in names(ac)) {
        if (!(a %in% c("names", "class"))) {
            attr(current, a) <- ac[[a]]
        }
    }

    ans <- all.equal(target, current, ...)

    f <- all.equal(ft, fc)
    if (!isTRUE(f)) {
        if (isTRUE(ans)) {
            ans <- character()
        }
        ans <- c(ans, paste("Filter:", f))
    }

    ans
}


as.Date.corpus_text <- function(x, format, ...)
{
    as.Date(as.character(x), format, ...)
}


as.data.frame.corpus_text <- function(x, row.names = NULL,
                                      optional = FALSE, ...)
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

    names(x) <- NULL
    value <- list(x)
    if (!optional)  {
        names(value) <- nm
    }

    structure(value, row.names = row.names, class = "data.frame")
}


as.matrix.corpus_text <- function(x, ...)
{
    stop("'as.matrix' is invalid for text objects")
}


as.vector.corpus_text <- function(x, mode = "any")
{
    if (mode == "any") {
        names(x) <- NULL
        ans <- x
    } else {
        ans <- as.vector(as.character(x), mode = mode)
    }
    ans
}


cbind.corpus_text <- function(..., deparse.level = 1)
{
    stop("'cbind' is invalid for text objects")
}


format.corpus_text <- function(x, trim = FALSE, chars = NULL,
                               justify = "left", width = NULL,
                               na.encode = TRUE, quote = FALSE,
                               na.print = NULL, print.gap = NULL,
                               ...)
{
    with_rethrow({
        x <- as_text(x)
        trim <- as_option("trim", trim)
        chars <- as_chars("chars", chars)
        justify <- as_justify("justify", justify)
        width <- as_integer_scalar("width", width)
        na.encode <- as_option("na.encode", na.encode)
        quote <- as_option("quote", quote)
        na.print <- as_na_print("na.print", na.print)
        print.gap <- as_print_gap("print.gap", print.gap)
    })

    utf8 <- Sys.getlocale("LC_CTYPE") != "C"

    if (is.null(chars)) {
        linewidth <- getOption("width")
        ellipsis <- if (utf8) 1 else 3
        quotes <- if (quote) 2 else 0
        gap <- if (is.null(print.gap)) 1 else print.gap

        names <- names(x)
        if (is.null(names)) {
            n <- length(x)
            if (n == 0) {
                namewidth <- 0
            } else {
                namewidth <- (floor(log10(n)) + 1) + 2 # digits + len("[]")
            }
        } else {
            namewidth <- max(0, utf8_width(names))
        }
        chars <- (linewidth - ellipsis - quotes - gap - namewidth)
        chars <- max(chars, 12)
    }

    fmt <- .Call(C_format_text, x, trim, chars, justify, width, na.encode,
                 quote, na.print, utf8)
    names(fmt) <- names(x)
    fmt
}


print.corpus_text <- function(x, rows = 20L, chars = NULL, quote = FALSE,
                              na.print = NULL, print.gap = NULL,
                              max = NULL, display = TRUE, ...)
{
    if (is.null(x)) {
        return(invisible(NULL))
    }

    if (!is_text(x)) {
        stop("argument is not a text vector")
    }

    with_rethrow({
        rows <- as_rows("rows", rows)
        chars <- as_chars("chars", chars)
        quote <- as_option("quote", quote)
        na.print <- as_na_print("na.print", na.print)
        print.gap <- as_print_gap("print_gap", print.gap)
        max <- as_max_print("max", max)
        display <- as_option("display", display)
    })

    if (length(x) == 0) {
        cat("text vector with 0 entries\n")
        return(invisible(x))
    }

    if (is.null(rows) || rows < 0) {
        rows <- .Machine$integer.max
    }

    if (is.null(print.gap)) {
        print.gap <- 1L
    }

    if (is.null(max)) {
        max <- getOption("max.print")
    }

    width <- getOption("width")
    stdout <- as.integer(stdout()) == 1

    if (length(x) <= rows) {
        xsub <- x
        nextra <- 0
    } else {
        xsub <- x[seq_len(rows)]
        nextra <- length(x) - rows
    }

    fmt <- format.corpus_text(xsub, chars = chars, quote = quote,
                              na.print = na.print, print.gap = print.gap)
    fmt <- utf8_encode(fmt, display)

    mat <- cbind(fmt)
    if (is.null(rownames(mat))) {
        rownames(mat) <- format(paste0("[", seq_len(nrow(mat)), "]"),
                                justify = "right")
    }
    colnames(mat) <- NULL
    .Call(C_print_table, mat, print.gap, FALSE, max, width, stdout)

    if (nextra > 0) {
        name_width <- max(0, utf8_width(rownames(mat)))

        ellipsis <- ifelse(Sys.getlocale("LC_CTYPE") == "C", "...", "\u22ee")
        ellipsis <- substr(ellipsis, 1, name_width)

        space <- format(ellipsis, width = name_width + print.gap)
        cat(sprintf("%s(%d entries total)\n", space, length(x)))
    }

    invisible(x)
}


rbind.corpus_text <- function(..., deparse.level = 1)
{
    stop("'rbind' is invalid for text objects")
}


solve.corpus_text <- function(a, b, ...)
{
    stop("'solve' is invalid for text objects")
}


summary.corpus_text <- function(object, ...)
{
    value <- c(Length = length(object), Class = "text", Mode = "character")
    class(value) <- c("summaryDefault", "table")
    value
}


t.corpus_text <- function(x)
{
    stop("'t' is invalid for text objects")
}

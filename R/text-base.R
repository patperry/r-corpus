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
        stop(gettextf("unary %s is not defined for text objects",
            .Generic), domain = NA)
    boolean <- switch(.Generic, `<` = , `>` = , `==` = , `!=` = ,
        `<=` = , `>=` = TRUE, FALSE)
    if (!boolean)
        stop(gettextf("%s is not defined for text objects",
            .Generic), domain = NA)
    e1 <- structure(as.character(e1), names = names(e1))
    e2 <- structure(as.character(e2), names = names(e2))
    NextMethod(.Generic)
}


all.equal.corpus_text <- function(target, current, ..., check.attributes = TRUE)
{
    if (is.null(target) && is.null(current)) {
        return(TRUE)
    }

    if (!is_corpus_text(target)) {
        stop("'target' is not a valid text object")
    }

    if (!inherits(current, "corpus_text")) {
        return(sprintf("target is corpus_text, current is %s",
                       class(current)[[1]]))
    }

    msg <- NULL
    if (check.attributes) {
        msg <- attr.all.equal(target, current, ..., check.names = FALSE)
    }

    fmsg <- all.equal(text_filter(target), text_filter(current))
    if (!isTRUE(fmsg)) {
        msg <- c(msg, paste("Filter:", fmsg))
    }

    xmsg <- all.equal(structure(as.character(target), names = names(target)),
                      structure(as.character(current), names = names(current)),
                      ...)

    if (!isTRUE(xmsg)) {
        msg <- c(msg, xmsg)
    }

    if (is.null(msg))
        TRUE
    else msg
}


as.Date.corpus_text <- function(x, format, ...)
{
    as.Date(as.character(x), format, ...)
}


as.data.frame.corpus_text <- as.data.frame.vector


as.matrix.corpus_text <- function(x, ...)
{
    stop("'as.matrix' is not defined for text objects")
}


as.vector.corpus_text <- function(x, mode = "any")
{
    as.vector(as.character(x), mode)
}


cbind.corpus_text <- function(..., deparse.level = 1)
{
    stop("'cbind' is not defined for text objects")
}


format.corpus_text <- function(x, trim = FALSE, chars = NULL,
                               justify = "left", width = NULL,
                               na.encode = TRUE, quote = FALSE,
                               na.print = NULL, print.gap = NULL, ...)
{
    with_rethrow({
        x <- as_corpus_text(x)
        trim <- as_option("trim", trim)
        chars <- as_chars("chars", chars)
        justify <- as_justify("justify", justify)
        width <- as_integer_scalar("width", width)
        na.encode <- as_option("na.encode", na.encode)
        quote <- as_option("quote", quote)
        na.print <- as_na_print("na.print", na.print)
        print.gap <- as_print_gap("print.gap", print.gap)
    })

    trunc <- if (is.null(chars)) getOption("width") else chars
    right <- (justify == "right")
    x <- .Call(C_text_trunc, x, trunc, right)

    fmt <- utf8_format(x, trim = trim, chars = chars, justify = justify,
                       width = width, na.encode = na.encode, quote = quote,
                       na.print = na.print, print.gap = print.gap)
    names(fmt) <- names(x)
    fmt
}


print.corpus_text <- function(x, rows = 20L, chars = NULL, quote = TRUE,
                              na.print = NULL, print.gap = NULL,
                              max = NULL, display = TRUE, ...)
{
    if (is.null(x)) {
        return(invisible(NULL))
    }

    if (!is_corpus_text(x)) {
        stop("argument is not a valid text object")
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

    if (is.null(max)) {
        max <- getOption("max.print")
    }

    if (is.null(print.gap)) {
        print.gap <- 1L
    }

    width <- getOption("width")
    utf8 <- output_utf8()

    if (is.null(chars)) {
        ellipsis <- if (utf8) 1 else 3
        quotes <- if (quote) 2 else 0
        names <- names(x)
        if (is.null(names)) {
            gap <- print.gap
            n <- length(x)
            if (n == 0) {
                namewidth <- 0
            } else {
                namewidth <- (floor(log10(n)) + 1) + 2 # digits + len("[]")
            }
        } else {
            gap <- 0
            namewidth <- 0
        }
        chars <- max(24L, width - ellipsis - quotes - gap - namewidth)
    }

    if (length(x) <= rows) {
        xsub <- x
        nextra <- 0
    } else {
        xsub <- x[seq_len(rows)]
        nextra <- length(x) - rows
    }

    fmt <- format.corpus_text(xsub, trim = TRUE, chars = chars,
                              quote = quote, na.print = na.print,
                              print.gap = print.gap)
    if (length(fmt) > 0) {
        utf8_print(fmt, chars = .Machine$integer.max, quote = quote,
                   na.print = na.print, print.gap = print.gap, max = max,
                   names = style_bold, rownames = style_faint,
                   escapes = style_faint, display = display)
    }

    if (nextra > 0) {
        if (length(fmt) == 0) {
            cat(sprintf("(%d entries total)\n", length(x)))
        } else if (is.null(names(fmt))) {
            # [XX]
            ellipsis <- ifelse(utf8, "\u22ee", "...")
            ellipsis <- format(substr(ellipsis, 1, namewidth - 1),
                               width = namewidth - 1, justify = "right")
            space <- format(ellipsis, width = namewidth + print.gap)
            if (output_ansi()) {
                space <- paste0("\x1b[", style_faint, "m", space, "\x1b[0m")
            }
            cat(space, sprintf("(%d entries total)\n", length(x)), sep = "")
        } else {
            ellipsis <- ifelse(utf8, "\u2026", "...")
            if (output_ansi()) {
                ellipsis <- paste0("\x1b[1m", ellipsis, "\x1b[0m")
            }
            cat(sprintf("%s (%d entries total)\n", ellipsis, length(x)))
        }
    }

    invisible(x)
}


rbind.corpus_text <- function(..., deparse.level = 1)
{
    stop("'rbind' is not defined for text objects")
}


solve.corpus_text <- function(a, b, ...)
{
    stop("'solve' is not defined for text objects")
}


summary.corpus_text <- function(object, ...)
{
    ntok <- text_ntoken(object)
    ntype <- text_ntype(object, collapse = TRUE)

    isna <- is.na(ntok)
    value <- summary(ntok[!isna], ...)
    nas <- sum(isna)

    names(value) <- paste(names(value), "Toks.")
    value[["Types"]] <- ntype
    if (nas > 0) {
        value[["NA's"]] <- nas
    }

    class(value) <- c("summaryDefault", "table")
    value
}


t.corpus_text <- function(x)
{
    stop("'t' is not defined for text objects")
}

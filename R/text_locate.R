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


text_count <- function(x, terms, filter = NULL, ...)
{
    with_rethrow({
        x <- as_corpus_text(x, filter, ...)
        terms <- as_character_vector("terms", terms)
    })
    .Call(C_text_count, x, terms)
}


text_detect <- function(x, terms, filter = NULL, ...)
{
    with_rethrow({
        x <- as_corpus_text(x, filter, ...)
        terms <- as_character_vector("terms", terms)
    })
    .Call(C_text_detect, x, terms)
}


text_subset <- function(x, terms, filter = NULL, ...)
{
    with_rethrow({
        x <- as_corpus_text(x, filter, ...)
    })
    i <- text_detect(x, terms)
    x[i]
}


text_match <- function(x, terms, filter = NULL, ...)
{
    with_rethrow({
        x <- as_corpus_text(x, filter, ...)
    })

    if (!(is.null(terms) || is.character(terms))) {
        stop("'terms' must be a character vector or NULL")
    }

    if (anyNA(terms)) {
        stop("'terms' argument cannot contain missing values")
    }

    if (!all(utf8_valid(terms))) {
        stop("'terms' argument cannot contain invalid UTF-8")
    }
    uterms <- as_utf8(terms)

    ans <- .Call(C_text_match, x, uterms)

    if (nlevels(ans$term) != length(terms)) {
        stop("'terms' argument cannot contain duplicate types")
    }

    if (!is.null(terms)) {
        levels(ans$term) <- terms
    }

    ans$text <- structure(ans$text, levels = labels(x), class = "factor")
    ans
}


text_locate <- function(x, terms, filter = NULL, random = FALSE, ...)
{
    if (!missing(random)) {
        warning("argument 'random' is deprecated; use the 'text_sample' function instead.",
                call. = FALSE)
        if (random) {
            return(text_sample(x, terms, filter = filter, ...))
        }
    }

    with_rethrow({
        x <- as_corpus_text(x, filter, ...)
        terms <- as_character_vector("terms", terms)
        random <- as_option("random", random)
    })

    ans <- .Call(C_text_locate, x, terms)
    ans$text <- structure(ans$text, levels = labels(x), class = "factor")
    if (random) {
        o <- sample.int(nrow(ans))
        ans <- ans[o,]
    }
    ans
}


text_sample <- function(x, terms, size = NULL, filter = NULL, ...)
{
    with_rethrow({
        x <- as_corpus_text(x, filter, ...)
        terms <- as_character_vector("terms", terms)
        size <- as_nonnegative("size", size)
    })

    loc <- text_locate(x, terms, filter,, ...)
    nloc <- nrow(loc)
    if (is.null(size)) {
        size <- nloc
    }
    o <- sample.int(nloc, min(size, nloc))
    ans <- loc[o,]
    row.names(ans) <- NULL
    ans
}


format.corpus_text_locate <- function(x, width = getOption("width"),
                                      print.gap = NULL, ...,
                                      display = FALSE, justify = "left")
{
    with_rethrow({
        width <- as_integer_scalar("width", width)
        print.gap <- as_print_gap("print.gap", print.gap)
        display <- as_option("display", display)
        justify <- as_justify("justify", justify)
    })

    if (is.null(print.gap)) {
        print.gap <- 1
    }

    nctx <- 0
    rval <- list()
    colwidths <- c()
    charmax <- .Machine$integer.max
    names <- names(x)

    for (i in seq_along(x)) {
        nm <- names[[i]]
        if (nm %in% c("before", "after")) {
            rval[[i]] <- NA # set to NA and deal with this later
            colwidths[[i]] <- 0
            nctx <- nctx + 1
        } else {
            w = utf8_width(nm)
            j <- justify
            if (nm == "text") {
                # use as_corpus_text to format an integer text id like
                # a character label
                rval[[i]] <- format(as_corpus_text(x[[i]]), width = 4,
                                    chars = charmax, display = display,
                                    justify = j)
            } else if (nm == "instance") {
                j <- "centre"
                rval[[i]] <- format(as_corpus_text(x[[i]]), width = 8,
                                    chars = charmax, display = display,
                                    justify = j)
            } else {
                rval[[i]] = format(x[[i]], width = w, ...,
                                   display = display, justify = j)
            }
            colwidths[[i]] <- max(w, utf8_width(rval[[i]]))
            names[[i]] <- format(nm, width = colwidths[[i]],
                                 chars = charmax, justify = j)
        }
    }

    row_names <- format(as_corpus_text(rownames(x)), width = 0, chars = charmax,
                        display = display, jusitfy = "left")
    colwidths <- c(colwidths, max(0, utf8_width(row_names)))

    extra <- width - sum(colwidths) - print.gap * (length(colwidths) - 1)
    ctxwidth <- max(12, extra / max(1, nctx))
    ellipsis <- ifelse(Sys.getlocale("LC_CTYPE") == "C", 3, 1)

    if ("before" %in% names) {
        i <- match("before", names)
        w <- floor(ctxwidth)
        rval[[i]] <- format(x[[i]], chars = w - ellipsis, width = w,
                            display = display, justify = "right")
        names[[i]] <- format("before", width = w, justify = "centre")
    }

    if ("after" %in% names) {
        i <- match("after", names)
        w <- ceiling(ctxwidth)
        rval[[i]] <- format(x[[i]], chars = w - ellipsis, width = w,
                            display = display, justify = "left")
        names[[i]] <- format("after", width = w, justify = "centre")
    }
    names(rval) <- names

    for (i in seq_along(rval)) {
        oldClass(rval[[i]]) <- "AsIs"
    }

    ans <- structure(rval, class = c("corpus_frame", "data.frame"),
                     row.names = row_names)
    ans
}


print.corpus_text_locate <- function(x, rows = 20L, print.gap = NULL,
                                     display = TRUE, ...)
{
    if (!is.data.frame(x)) {
        stop("argument is not a data frame")
    }
    n <- nrow(x)

    with_rethrow({
        rows <- as_rows("rows", rows)
        print.gap <- as_print_gap("print_gap", print.gap)
        display <- as_option("display", display)
    })

    if (is.null(rows) || rows < 0) {
        rows <- .Machine$integer.max
    }

    trunc <- (!is.null(rows) && n > rows)
    if (trunc) {
        xsub <- x[seq_len(rows), , drop = FALSE]
    } else {
        xsub <- x
    }

    fmt <- format.corpus_text_locate(xsub, print.gap = print.gap,
                                     display = display, ...)
    print.corpus_frame(fmt, rows = .Machine$integer.max,
                       chars = .Machine$integer.max, print.gap = print.gap)
    if (trunc) {
        name_width <- max(0, utf8_width(rownames(fmt)))

        ellipsis <- ifelse(Sys.getlocale("LC_CTYPE") == "C", ".", "\u22ee")
        ellipsis <- substr(ellipsis, 1, name_width)
        gap <- if (is.null(print.gap)) 1 else print.gap

        space <- format(ellipsis, width = name_width + gap)
        cat(sprintf("%s(%d rows total)\n", space, n))
    }

    invisible(x)
}

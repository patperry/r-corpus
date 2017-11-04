\name{print.corpus_frame}
\title{Corpus Data Frame Printing}
\alias{format.corpus_frame}
\alias{print.corpus_frame}
\description{
Printing and formatting corpus data frames.
}
\usage{
\method{print}{corpus_frame}(x, rows = 20L, chars = NULL, digits = NULL,
      quote = FALSE, na.print = NULL, print.gap = NULL,right = FALSE,
      row.names = TRUE, max = NULL, display = TRUE, ...)

\method{format}{corpus_frame}(x, chars = NULL, na.encode = TRUE, quote = FALSE,
       na.print = NULL, print.gap = NULL, ..., justify = "none")
}
\arguments{
    \item{x}{data frame object to print or format.}

    \item{rows}{integer scalar giving the maximum number of rows to print
        before truncating the output. A negative or missing value indicates
        no upper limit.}

    \item{chars}{maximum number of character units to display; see
        \code{\link{utf8_format}}.}

    \item{digits}{minimal number of significant digits; see
        \code{\link{print.default}}.}

    \item{quote}{logical scalar indicating whether to put surrounding
        double-quotes (\code{'"'}) around character strings and escape
        internal double-quotes.}

    \item{na.print}{character string (or \code{NULL}) indicating
        the encoding for \code{NA} values. Ignored when
        \code{na.encode} is \code{FALSE}.}

    \item{print.gap}{non-negative integer (or \code{NULL}) giving the
        number of spaces in gaps between columns; set to \code{NULL}
        or \code{1} for a single space.}

    \item{right}{logical indicating whether to right-align columns
        (ignored for text, character, and factor columns).}

    \item{row.names}{logical indicating whether to print row names, or
        a character vector giving alternate row names to display.}

    \item{max}{maximum number of entries to print; defaults to
        \code{getOption("max.print")}.}

    \item{display}{logical scalar indicating whether to optimize the
        printing for display, not byte-for-byte data transmission;
        see \code{utf8_encode}.}

    \item{justify}{justification; one of \code{"left"}, \code{"right"},
        \code{"centre"}, or \code{"none"}. Can be abbreviated.}

    \item{na.encode}{logical scalar indicating whether to encode
         \code{NA} values as character strings.}

    \item{...}{further arguments passed to or from other methods.}
}
\details{
    The \code{"corpus_frame"} class is a subclass of \code{"data.frame"},
    overriding the default print and format methods. To apply this
    class to a data frame, set is class to
    \code{c("corpus_frame", "data.frame")}.

    Corpus frame printing left-justifies character and text columns,
    truncates the output, and displays emoji on Mac OS.
}
\seealso{
\code{\link{corpus_frame}}, \code{\link{print.data.frame}},
\code{\link{utf8_print}}
}
\examples{
# default data frame printing
x <- data.frame(text = c("hello world", intToUtf8(0x1f638 + 0:3), letters))
print(x)

# corpus frame printing
y <- x
class(y) <- c("corpus_frame", "data.frame")
print(y)

print(y, 10) # change truncation limit
}

\name{utf8}
\title{UTF-8 Text}
\alias{utf8}
\alias{as_utf8}
\alias{utf8_encode}
\alias{utf8_format}
\alias{utf8_valid}
\alias{utf8_width}
\description{
    UTF-8 text conversion and formatting.
}
\usage{
    as_utf8(x)

    utf8_encode(x, display = FALSE)

    utf8_format(x, trim = FALSE, chars = NULL, justify = "left",
                width = NULL, na.encode = TRUE, ...)

    utf8_width(x, encode = TRUE)

    utf8_valid(x)
}
\arguments{
    \item{x}{character object}

    \item{display}{logical scalar indicating whether to optimize the
        encoding for display, not lossless communication.}

    \item{trim}{logical scalar indicating whether to suppress
        padding spaces around elements.}

    \item{chars}{integer scalar indicating the maximum number of
        character units to display.  Wide characters like emoji take
        two character units; combining marks and default ignorables
        take none. Longer strings get truncated and suffixed or prefixed
        with an ellipsis (\code{"..."} in C locale, \code{"\u2026"} in
        others).}

    \item{justify}{justification; one of \code{"left"}, \code{"right"},
       \code{"centre"}, or \code{"none"}. Can be abbreviated.}

   \item{width}{the minimum field width; set to \code{NULL} or
       \code{0} for no restriction.}

   \item{na.encode}{logical scalar indicating whether to incode
       \code{NA} values as \code{"NA"}.}

   \item{...}{further arguments passed from other methods. Ignored.}

   \item{encode}{whether to encode the object before measuring its width.}
}
\details{
    \code{as_utf8} converts a character object from its declared encoding
    to a valid UTF-8 character object, or throws an error if no conversion
    is possible.

    \code{utf8_encode} encodes a character object for printing on a UTF-8
    device by escaping controls characters and other non-printable
    characters. When \code{display = TRUE}, the function optimizes the
    encoding for display by removing default ignorable characters (soft
    hyphens, zero-width spaces, etc.) and placing zero-width spaces after
    wide emoji. When \code{LC_CTYPE = "C"}, this function escapes all
    non-ASCII characters.

    \code{utf8_format} formats a character object for printing, optionally
    truncating long character strings. Unlike the base R function,
    setting \code{justify = "none"} produces identical behavior to
    setting \code{justify = "left"}; to recover the base R behavior,
    set \code{justify = "none"} and \code{trim = TRUE}.

    \code{utf8_valid}{tests whether the elements of a character object
    can be translated to valid UTF-8 strings.}

    \code{utf8_width} returns the printed widths of the elements of
    a character object on a UTF-8 device or, when \code{LC_CTYPE = "C"},
    on an ASCII device. If the string is not printable on the device,
    for example if it contains a control code like \code{"\n"}, then
    the result is \code{NA}. If \code{encode = TRUE}, the default,
    then the function returns the widths of the encoded elements
    (via \code{utf8_encode}); otherwise, the function returns the
    widths of the original elements.
}
\value{
    For \code{as_utf8} or \code{utf8_encode}, a character object with
    the same attributes as \code{x} but with \code{Encoding} set to
    \code{"UTF-8"}.

    For \code{utf8_valid} or \code{utf8_width}, a logical or integer
    object, respectively, with the same \code{names}, \code{dim}, and
    \code{dimnames} as \code{x}.
}
\seealso{
    \code{\link{as_text}}, \code{\link{iconv}}.
}
\examples{
    # the second element is encoded in latin-1, but declared as UTF-8
    x <- c("fa\u00E7ile", "fa\xE7ile", "fa\xC3\xA7")
    Encoding(x) <- c("UTF-8", "UTF-8", "bytes")

    # attempt to convert to UTF-8 (fails)
    \dontrun{as_utf8(x)}

    y <- x
    Encoding(y[2]) <- "latin1" # mark the correct encoding
    as_utf8(y) # succeeds

    # test for valid UTF-8
    utf8_valid(x)

    # encoding
    utf8_encode(x)

    # formatting
    utf8_format(x, chars = 3)
    utf8_format(x, chars = 3, justify = "centre", width = 10)
    utf8_format(x, chars = 3, justify = "right")

    # get widths
    utf8_width(x)
    utf8_width(x, encode = FALSE)
}

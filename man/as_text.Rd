\name{as_text}
\alias{as_text}
\alias{as_text.character}
\alias{as_text.corpus_json}
\alias{as_text.corpus_text}
\alias{as_text.data.frame}
\alias{as_text.default}
\alias{corpus_text}
\alias{is_text}
\title{Text Vectors}
\description{
    Create or test for text objects.
}
\usage{
as_text(x, names = NULL, filter = NULL, ...)

is_text(x)
}
\arguments{
    \item{x}{object to be coerced or tested.}

    \item{names}{character vector of names for the converted text.}

    \item{filter}{text filter object for the converted text.}

    \item{\dots}{further arguments passed to or from other methods.}
}
\details{
    The \code{corpus_text} type is a new data type provided by the
    \code{corpus} package suitable for processing Unicode text. Text
    vectors behave like character vectors (and can be converted to them
    with the \code{as.character} function). They can be created using the
    \code{\link{read_ndjson}} function or by converting another object
    using the \code{as_text} function.

    All text objects have a \code{\link{text_filter}} property specify
    how to transform the text into tokens or segment it into sentences.

    The default behavior for \code{as_text} is to proceed as follows:
    \enumerate{
        \item If \code{x} is a \code{character} vector, then we create
            a new \code{text} vector from \code{x}.

        \item If \code{x} is a data frame, then we call \code{as_text}
            on \code{x$text} if a column named \code{"text"} exists in
            the data frame.  If the data frame does not have a column
            named \code{"text"}, then we fail with an error message.

        \item If \code{x} is a \code{corpus_text} object, then we drop all
            attributes and we set the class to \code{"corpus_text"}.

        \item The default behavior for when none of the above conditions
            are true is to call \code{as.character} on the object first,
            preserving the names, and then and call \code{as_text} on
            the returned character object.
    }

    In all cases, when the \code{names} is missing, we set the result
    names to \code{names(x)} (or \code{rownames(x)} for a data frame
    argument). When \code{names} is not missing, we set the result names
    to this value.

    Similarly, when \code{filter} is missing, we set the result text filter
    to \code{text_filter(x)}. When \code{filter} is not missing, we set
    the result text filter to this value.

    Note that the special handling for the names of the object is different
    from the other R conversion functions (\code{as.numeric},
    \code{as.character}, etc.), which drop the names.

    \code{as_text} is generic: you can write methods to handle specific
    classes of objects.
}
\value{
    \code{as_text} attempts to coerce its argument to \code{text} type and
        set its \code{names} and \code{text_filter} properties; it strips
        all other attributes.

    \code{is_text} returns \code{TRUE} or \code{FALSE} depending on
        whether its argument is of text type or not.
}
\seealso{
    \code{\link{as_utf8}}, \code{\link{text_filter}},
    \code{\link{read_ndjson}}.
}
\examples{
    as_text("hello, world!")
    as_text(c(a = "goodnight", b = "moon")) # keeps names
    as_text(c(a = "goodnight", b = "moon"), names = NULL) # drops names

    is_text("hello") # FALSE, "hello" is character, not text
}
\keyword{classes}

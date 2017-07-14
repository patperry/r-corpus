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
    as_text(x, filter = text_filter(x), ...)
    is_text(x)
}
\arguments{
    \item{x}{object to be coerced or tested.}

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
            a new \code{text} vector from \code{x}, preserving
            \code{names(x)} if they exist.

        \item If \code{x} is a data frame, then we call \code{as_text}
            on \code{x$text} if a column named \code{"text"}
            exits in the data frame. We set the names of the result to
            the data frame's row names, if they exist. If the data frame
            does not have a column named \code{"text"}, then we fail
            with an error message.

        \item If \code{x} is a \code{corpus_text} object,  then we drop all
            attributes from the object except for its names and
            filter, and we set the object class to \code{corpus_text}.

        \item The default behavior for when none of the above conditions
            are true is to call \code{as.character} on the object first,
            and call \code{as_text} on the returned character object.
    }
    In all cases, we set the \code{text_filter} property of the result
    to the \code{filter} argument given to \code{as_text}.

    Note that the special handling for the names of the object is different
    from the other R conversion functions (\code{as.numeric},
    \code{as.character}, etc.), which drop the names.

    \code{as_text} is generic: you can write methods to handle specific
    classes of objects.
}
\value{
    \code{as_text} attempts to coerce its argument to \code{text} type and
        set its \code{text_filter} property; it strips all other attributes
        except for names.

    \code{is_text} returns \code{TRUE} or \code{FALSE} depending on
        whether its argument is of text type or not.
}
\seealso{
    \code{\link{as_utf8}}, \code{\link{text_filter}},
    \code{\link{read_ndjson}}.
}
\examples{
    as_text("hello, world!")
    as_text(c(a="goodnight", b="moon")) # keeps names

    is_text("hello") # FALSE, "hello" is character, not text
}
\keyword{classes}

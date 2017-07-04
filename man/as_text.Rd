\name{as_text}
\alias{as_text}
\alias{as_text.default}
\alias{corpus_text}
\alias{is_text}
\alias{is_text.default}
\title{Text Vectors}
\description{
    Create or test for text objects.
}
\usage{
    as_text(x, ...)
    is_text(x)
}
\arguments{
    \item{x}{object to be coerced or tested.}
    \item{\dots}{further arguments passed to or from other methods.}
}
\details{
    The \code{text} type is a new data type provided by the \code{corpus}
    package suitable for processing Unicode text. Text vectors behave
    like character vectors (and can be converted to them with the
    \code{as.character} function). They can be created using the
    \code{\link{read_ndjson}} function or by converting another object
    using the \code{as_text} function.

    The default behavior for \code{as_text} is to proceed as follows:
    \enumerate{
        \item If \code{x} is a \code{character} vector, then we create
            a new \code{text} vector from \code{x}, preserving
            \code{names(x)} if they exist.

        \item If \code{is_text(x)} is \code{TRUE}, then we drop all
            attributes from the object except for its names, and we
            set the object class to \code{text}.

        \item Otherwise, if \code{is.data.frame(x)} is \code{TRUE},
            then we look for a column to convert. First, we look for a
            column named \code{"text"}. If none exists, we look for a
            column of type \code{text}. If we find such a column, then
            we call \code{as_text} on the found column and we set the
            object names to match \code{x}'s row names. If there
            are no columns with type \code{text} or if there multiple
            columns of type \code{text}, none of which are named
            \code{"text"}, then we fail with an error message.

        \item Finally, if \code{x} is not a \code{character} vector, and if
            \code{is_text(x)} and \code{is.data.frame(x)} are
            both \code{FALSE}, then we try to use \code{as.character}
            on the object and then we convert the resulting character
            vector to \code{text}.
    }
    This special handling for the names of the object is different from
    the other R conversion functions (\code{as.numeric},
    \code{as.character}, etc.), which drop the names.

    \code{as_text} is generic: you can write methods to handle specific
    classes of objects.
}
\value{
    \code{as_text} attempts to coerce its argument to \code{text} type; it
        strips all attributes except for names.

    \code{is_text} returns \code{TRUE} or \code{FALSE} depending on
        whether its argument is of text type or not.
}
\seealso{
    \code{\link{as_utf8}}, \code{\link{read_ndjson}}.
}
\examples{
    as_text("hello, world!")
    as_text(c(a="goodnight", b="moon")) # keeps names

    is_text("hello") # FALSE, "hello" is character, not text
}
\keyword{classes}

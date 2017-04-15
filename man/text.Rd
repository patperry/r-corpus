\name{text}
\alias{as.text}
\alias{as.text.default}
\alias{is.text}
\alias{is.text.default}
\alias{text}
\title{Text Vectors}
\description{
    Create or test for objects of type \code{"text"}.
}
\usage{
    as.text(x, ...)
    is.text(x)
    text(...)
}
\arguments{
    \item{x}{object to be coerced or tested.}
    \item{\dots}{further arguments passed to or from other methods.}
}
\details{
    The \code{text} type is a new datatype provided by the \code{corpus}
    package suitable for processing Unicode text. Text vectors behave
    like character vectors (and can be converted to them with the
    \code{as.character} function). They can be created using the
    \code{text} and \code{\link{read_json}} functions or by
    converting another object using the \code{as.text} function.

    \code{as.text} and \code{is.text} are generic: you can write methods
    to handle specific classes of objects. The default behavior is to
    call \code{as.character} on the object, then to convert the character
    vector to a \code{text} object.
}
\value{
    \code{as.text} attempts to coerce its argument to text type; it
        strips attributes including names.

    \code{is.text} returns \code{TRUE} or \code{FALSE} depending on
        whether its argument is of text type or not.
}
\seealso{
    \code{\link{read_json}}.
}
\examples{
    as.text("hello, world!")
    as.text(c(a="goodnight", b="moon")) # drops names
    text(a="goodnight", b="moon") # works like 'c', keeps names

    is.text("hello") # FALSE, "hello" is character, not text
}
\keyword{classes}

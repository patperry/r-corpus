\name{text}
\alias{as_text}
\alias{as_text.default}
\alias{is_text}
\alias{is_text.default}
\alias{text}
\title{Text Vectors}
\description{
    Create or test for objects of type \code{"text"}.
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
    The \code{text} type is a new datatype provided by the \code{corpus}
    package suitable for processing Unicode text. Text vectors behave
    like character vectors (and can be converted to them with the
    \code{as.character} function). They can be created using the
    \code{\link{read_json}} function or by converting another object
    using the \code{as_text} function.

    The \code{as_text} function first gets the names of the object
    by calling \code{names(x)}; then, it converts the object to type
    \code{"text"} and drops all of the object attributes. Finally,
    the function sets the converted objects names to original object
    names. This special handling for the names of the object is
    different from the other R conversion functions
    (\code{as.numeric}, \code{as.character}, etc.), which drop the names.

    \code{as_text} and \code{is_text} are generic: you can write methods
    to handle specific classes of objects. The default behavior is to
    extract the names from the object using the \code{names} function,
    then call \code{as.character} on the object and convert the
    resulting character vector to a \code{text} object.
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
    as_text("hello, world!")
    as_text(c(a="goodnight", b="moon")) # keeps names

    is_text("hello") # FALSE, "hello" is character, not text
}
\keyword{classes}

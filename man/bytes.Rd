\name{char_bytes}
\title{Character Encoding}
\alias{char_bytes}
\description{
    Get the raw bytes representing a character vector.
}
\usage{
char_bytes(x)
}
\arguments{
    \item{x}{character object.}
}
\details{
\code{char_bytes} converts a character vector to a list of raw vectors
with byte representation of the character vector elements.
}
\value{
A list the same length as \code{x}, with the same names. The entries
in the result give the bytes representing the corresponding entries in
the input, or \code{NULL} if the input entry is \code{NA}.
}
\seealso{
\code{\link{as_utf8}}, \code{\link{iconv}}.
}
\examples{
# the second element is encoded in latin-1, but declared as UTF-8
x <- c("fa\u00E7ile", "fa\xE7ile", "fa\xC3\xA7ile")
Encoding(x) <- c("UTF-8", "UTF-8", "bytes")
char_bytes(x)
}

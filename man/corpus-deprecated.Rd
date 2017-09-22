\name{corpus-deprecated}
\alias{corpus-deprecated}
\alias{text_length}
\title{Deprecated Functions in Package \pkg{corpus}}
\description{
These functions are provided for compatibility with older versions of
\pkg{corpus} only, and may be defunct as soon as the next release.
}
\usage{
text_length(x, filter = NULL, ...)
}
\arguments{
\item{x}{object to be tokenized.}

\item{filter}{if non-\code{NULL}, a text filter to to use instead of
    the default text filter for \code{x}.}

\item{\dots}{additional properties to set on the text filter.}
}
\details{
This function is deprecated. Use \code{\link{text_ntoken}} instead.
}
\seealso{
\code{\link{Deprecated}}
}
\keyword{internal}
\keyword{misc}

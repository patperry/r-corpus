\name{text_stats}
\alias{text_stats}
\title{Text Statistics}
\description{
    Report descriptive statistics for a set of texts.
}
\usage{
text_stats(x, filter = NULL, ...)
}
\arguments{
\item{x}{a text corpus.}

\item{filter}{if non-\code{NULL}, a text filter to to use instead of
    the default text filter for \code{x}.}

\item{\dots}{additional properties to set on the text filter.}
}
\details{
    \code{text_stats} reports descriptive statistics for a set of texts:
    the number of tokens, unique types, and sentences.
}
\value{
    A data frame with columns named \code{tokens}, \code{types}, and
    \code{sentences}, with one row for each text.
}
\seealso{
    \code{\link{text_filter}}, \code{\link{term_stats}}.
}
\examples{
text_stats(c("A rose is a rose is a rose.",
             "A Rose is red. A violet is blue!"))
}

\name{corpus-deprecated}
\alias{corpus-deprecated}
\alias{as_text}
\alias{is_text}
\alias{term_frame}
\title{Deprecated Functions in Package \pkg{corpus}}
\description{
These functions are provided for compatibility with older versions of
\pkg{corpus} only, and may be defunct as soon as the next release.
}
\usage{
as_text(x, filter = NULL, ..., names = NULL)

is_text(x)

term_frame(x, filter = NULL, weights = NULL,
            ngrams = NULL, select = NULL, group = NULL, ...)
}
\arguments{
\item{x}{a text vector to tokenize.}

\item{filter}{if non-\code{NULL}, a text filter to to use instead of
    the default text filter for \code{x}.}

\item{\dots}{text filter properties to set on the result.}

\item{names}{if non-\code{NULL} character vector of names for
    the converted result.}

\item{weights}{a numeric vector the same length of \code{x} assigning
    weights to each text, or \code{NULL} for unit weights.}

\item{ngrams}{an integer vector of n-gram lengths to include, or
    \code{NULL} to use the \code{select} argument to determine the
    n-gram lengths.}

\item{select}{a character vector of terms to count, or \code{NULL} to
    count all terms that appear in \code{x}.}

\item{group}{if non-\code{NULL}, a factor, character string, or
    integer vector the same length of \code{x} specifying the grouping
    behavior.}

\item{\dots}{additional properties to set on the text filter.}
}
\details{
These functions are deprecated; use \code{\link{as_corpus_text}},
\code{\link{is_corpus_text}}, or \code{\link{term_matrix}} instead.
}
\seealso{
\code{\link{Deprecated}}
}
\keyword{internal}
\keyword{misc}

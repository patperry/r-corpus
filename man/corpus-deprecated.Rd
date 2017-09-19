\name{corpus-deprecated}
\alias{corpus-deprecated}
\alias{as_corpus}
\alias{as_text}
\alias{is_corpus}
\alias{is_text}
\alias{term_frame}
\title{Deprecated Functions in Package \pkg{corpus}}
\description{
These functions are provided for compatibility with older versions of
\pkg{corpus} only, and may be defunct as soon as the next release.
}
\usage{
corpus(..., row.names = NULL, filter = NULL)

as_corpus(x, filter = NULL, ..., row.names = NULL)

as_text(x, filter = NULL, ..., names = NULL)

is_corpus(x)

is_text(x)

term_frame(x, filter = NULL, weights = NULL,
           ngrams = NULL, select = NULL, group = NULL, ...)
}
\arguments{
\item{x}{Deprecated.}

\item{filter}{Deprecated.}

\item{group}{Deprecated.}

\item{names}{Deprecated.}

\item{ngrams}{Deprecated.}

\item{row.names}{Deprecated.}

\item{select}{Deprecated.}

\item{weights}{Deprecated.}

\item{\dots}{Deprecated.}
}
\details{
These functions are deprecated; use \code{\link{corpus_frame}},
\code{\link{as_corpus_frame}}, \code{\link{as_corpus_text}},
\code{\link{is_corpus_frame}}, \code{\link{is_corpus_text}},
or \code{\link{term_matrix}} instead.
}
\seealso{
\code{\link{Deprecated}}
}
\keyword{internal}
\keyword{misc}

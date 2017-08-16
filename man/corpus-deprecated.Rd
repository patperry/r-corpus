\name{corpus-deprecated}
\alias{corpus-deprecated}
\alias{term_counts}
\title{Deprecated Functions in Package \pkg{corpus}}
\description{
  These functions are provided for compatibility with older versions of
  \pkg{corpus} only, and may be defunct as soon as the next release.
}
\usage{
    term_counts(x, filter = text_filter(x), weights = NULL,
                ngrams = NULL, min_count = NULL, max_count = NULL,
                min_support = NULL, max_support = NULL, types = FALSE)
}
\arguments{
    \item{x}{a text vector to tokenize.}

    \item{filter}{a token filter specifying the tokenization rules.}

    \item{weights}{a numeric vector the same length of \code{x} assigning
        weights to each text, or \code{NULL} for unit weights.}

    \item{ngrams}{an integer vector of n-gram lengths to include, or
        \code{NULL} for length-1 n-grams only.}

    \item{min_count}{a numeric scalar giving the minimum term count to include
        in the output, or \code{NULL} for no minimum count.}

    \item{max_count}{a numeric scalar giving the maximum term count to include
        in the output, or \code{NULL} for no maximum count.}

    \item{min_support}{a numeric scalar giving the minimum term support to
        include in the output, or \code{NULL} for no minimum support.}

    \item{max_support}{a numeric scalar giving the maximum term support to
        include in the output, or \code{NULL} for no maximum support.}

    \item{types}{a logical value indicating whether to include columns for
        the types that make up the terms.}
}
\details{
    The \code{term_counts} function is deprecated, renamed to
    \code{\link{term_stats}}.
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{internal}
\keyword{misc}

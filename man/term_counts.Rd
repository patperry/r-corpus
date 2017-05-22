\name{term_counts}
\alias{term_counts}
\title{Term Frequencies}
\description{
    Tokenize a set of texts and tabulate the term occurrence frequencies.
}
\usage{
    term_counts(x, filter = text_filter(), weights = NULL)
}
\arguments{
    \item{x}{a text vector to tokenize.}

    \item{filter}{a \code{text_filter} specifying the tokenization rules.}

    \item{weights}{a numeric vector the same length of \code{x} assigning
        weights to each text, or \code{NULL} for unit weights.}
}
\details{
    \code{term_counts} tokenizes a set of texts and computes the occurence
    counts for each term. If \code{weights} is non-\code{NULL}, then each
    token in text \code{i} increments the count for the corresponding term
    by \code{weights[i]}; otherwise, each appearance increments the count
    by one.
}
\value{
    A data frame with two columns, \code{term} and \code{count}, and one
    row for each appearing term. Rows are sorted in descending order according
    to \code{count}, with ties broken arbitrarily.
}
\examples{
    term_counts("A rose is a rose is a rose.")
}

\name{term_counts}
\alias{term_counts}
\title{Term Frequencies}
\description{
    Tokenize a set of texts and tabulate the term occurrence frequencies.
}
\usage{
    term_counts(x, filter = token_filter(), ngrams = 1, select = NULL,
                weights = NULL)
}
\arguments{
    \item{x}{a text vector to tokenize.}

    \item{filter}{a token filter specifying the tokenization rules.}

    \item{weights}{a numeric vector the same length of \code{x} assigning
        weights to each text, or \code{NULL} for unit weights.}
}
\details{
    \code{term_counts} tokenizes a set of texts and computes the occurrence
    counts for each term. If \code{weights} is non-\code{NULL}, then each
    token in text \code{i} increments the count for the corresponding term
    by \code{weights[i]}; otherwise, each appearance increments the count
    by one.
}
\value{
    A data frame with two columns, \code{term} and \code{count}, with one
    row for each appearing term. Rows are sorted in descending order
    according to \code{count}, with ties broken arbitrarily.
}
\seealso{
    \code{\link{tokens}}, \code{\link{term_matrix}}.
}
\examples{
    term_counts("A rose is a rose is a rose.")

    # remove punctuation and stop words
    term_counts("A rose is a rose is a rose.",
                token_filter(drop_symbol = TRUE, drop = stopwords("english")))

    # weight the texts
    term_counts(c("A rose is a rose is a rose.",
                  "A Rose is red, a violet is blue!"),
                weights = c(100, 1))
}

\name{term_counts}
\alias{term_counts}
\title{Term Frequencies}
\description{
    Tokenize a set of texts and tabulate the term occurrence frequencies.
}
\usage{
    term_counts(x, filter = token_filter(), weights = NULL,
                ngrams = NULL, min = NA, max = NA, limit = NA,
                types = FALSE)
}
\arguments{
    \item{x}{a text vector to tokenize.}

    \item{filter}{a token filter specifying the tokenization rules.}

    \item{weights}{a numeric vector the same length of \code{x} assigning
        weights to each text, or \code{NULL} for unit weights.}

    \item{ngrams}{an integer vector of n-gram lengths to include, or
        \code{NULL} for length-1 n-grams only.}

    \item{min}{a numeric scalar giving the minimum term count to include
        in the output, or \code{NA} for no minimum count.}

    \item{max}{a numeric scalar giving the maximum term count to include
        in the output, or \code{NA} for no maximum count.}

    \item{limit}{an integer scalar giving the maximum number of terms
        to include in the output, or \code{NA} for no maximum number of
        terms.}

    \item{types}{a logical value indicating whether to include columns for
        the types that make up the terms.}
}
\details{
    \code{term_counts} tokenizes a set of texts and computes the occurrence
    counts for each term. If \code{weights} is non-\code{NULL}, then each
    token in text \code{i} increments the count for the corresponding term
    by \code{weights[i]}; otherwise, each appearance increments the count
    by one.

    To include multi-type terms, specify the designed term lengths using
    the \code{ngrams} argument.
}
\value{
    A data frame with columns named \code{term} and \code{count}, with one
    row for each appearing term. Rows are sorted in descending order
    according to \code{count}, with ties broken lexicographically using
    the \code{term}.

    If \code{types = TRUE}, then the result also includes columns named
    \code{type1}, \code{type2}, etc. for the types that make up the
    term.
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

    # unigrams, bigrams, and trigrams
    term_counts("A rose is a rose is a rose.", ngrams = 1:3)

    # also include the type information
    term_counts("A rose is a rose is a rose.", ngrams = 1:3, types = TRUE)
}

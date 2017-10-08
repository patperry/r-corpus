\name{term_stats}
\alias{term_stats}
\title{Term Statistics}
\description{
    Tokenize a set of texts and tabulate the term occurrence statistics.
}
\usage{
term_stats(x, filter = NULL, ngrams = NULL,
           min_count = NULL, max_count = NULL,
           min_support = NULL, max_support = NULL, types = FALSE,
           subset, ...)
}
\arguments{
\item{x}{a text vector to tokenize.}

\item{filter}{if non-\code{NULL}, a text filter to to use instead of
    the default text filter for \code{x}.}

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

\item{subset}{logical expression indicating elements or rows to keep:
    missing values are taken as false.}

\item{\dots}{additional properties to set on the text filter.}
}
\details{
    \code{term_stats} tokenizes a set of texts and computes the occurrence
    counts and supports for each term. The \sQuote{count} is the number of
    occurrences of the term across all texts; the \sQuote{support} is the
    number of texts containing the term. Each appearance of a term
    increments its count by one. Likewise, an appearance of a term in text
    \code{i} increments its support once, not for each occurrence
    in the text.

    To include multi-type terms, specify the designed term lengths using
    the \code{ngrams} argument.
}
\value{
    A data frame with columns named \code{term}, \code{count}, and
    \code{support}, with one row for each appearing term. Rows are sorted
    in descending order according to \code{support} and then \code{count},
    with ties broken lexicographically by \code{term}, using the
    character ordering determined by the current locale
    (see \code{\link{Comparison}} for details).

    If \code{types = TRUE}, then the result also includes columns named
    \code{type1}, \code{type2}, etc. for the types that make up the
    term.
}
\seealso{
    \code{\link{text_tokens}}, \code{\link{term_matrix}}.
}
\examples{
term_stats("A rose is a rose is a rose.")

# remove punctuation and English stop words
term_stats("A rose is a rose is a rose.",
           text_filter(drop_symbol = TRUE, drop = stopwords_en))

# unigrams, bigrams, and trigrams
term_stats("A rose is a rose is a rose.", ngrams = 1:3)

# also include the type information
term_stats("A rose is a rose is a rose.", ngrams = 1:3, types = TRUE)
}

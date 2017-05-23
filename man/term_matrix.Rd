\name{term_matrix}
\alias{term_matrix}
\title{Term Frequency Matrix}
\description{
    Tokenize a set of texts and compute a term frequency matrix, with
    one column for each term.
}
\usage{
    term_matrix(x, filter = text_filter(), weights = NULL, group = NULL)
}
\arguments{
    \item{x}{a text vector to tokenize.}

    \item{filter}{a \code{text_filter} specifying the tokenization rules.}

    \item{weights}{a numeric vector the same length of \code{x} assigning
        weights to each text, or \code{NULL} for unit weights.}

    \item{group}{if non-\code{NULL}, a factor, character string, or
        integer vector the same length of \code{x} specifying the grouping
        behavior.}
}
\details{
    \code{term_matrix} tokenizes a set of texts and computes the occurence
    counts for each term. If \code{weights} is non-\code{NULL}, then each
    token in text \code{i} increments the count for the corresponding term
    by \code{weights[i]}; otherwise, each appearance increments the count
    by one.

    If \code{group} is \code{NULL}, then the output has one row for each
    input text. Otherwise, we convert \code{group} to a \code{factor} and
    compute one row for each level. Texts with \code{NA} values for
    \code{group} get skipped.
}
\value{
    A sparse matrix in \code{"dgCMatrix"} format with one column for each term
    and one row for each input text or (if \code{group} is non-\code{NULL}) on
    row for each grouping level.

    If \code{filter$select} is non-\code{NULL}, then the column names will
    be equal to \code{filter$select}. Otherwise, the columns are assigned
    in arbitrary order.
}
\seealso{
    \code{\link{tokens}}, \code{\link{term_counts}}.
}
\examples{
    text <- c("A rose is a rose is a rose.",
              "A Rose is red, a violet is blue!",
              "A rose by any other name would smell as sweet.")
    term_matrix(text)

    # select certain terms
    f <- text_filter(select = c("rose", "red", "violet", "sweet"))
    term_matrix(text, f)

    # specify a grouping factor
    term_matrix(text, f, group = c("Good", "Bad", "Good"))

    # weight the texts
    term_matrix(text, f, weights = c(1, 2, 10),
                group = c("Good", "Bad", "Good"))
}

\name{text_types}
\alias{text_ntype}
\alias{text_types}
\title{Text type sets.}
\description{
    Get or measure the set of types (unique token values).
}
\usage{
    text_types(x, filter = token_filter(), weights = NULL, group = NULL,
               collapse = FALSE)

    text_ntype(x, filter = token_filter(), weights = NULL, group = NULL,
               collapse = FALSE)
}
\arguments{
    \item{x}{a text or character vector.}

    \item{filter}{a token filter defining the token boundaries and
        normalization.}

    \item{weights}{a numeric vector the same length of \code{x} assigning
        weights to each text, or \code{NULL}.}

    \item{group}{if non-\code{NULL}, a factor, character string, or
        integer vector the same length of \code{x} specifying the grouping
        behavior.}

    \item{collapse}{a logical value indicating whether to collapse the
        aggregation over all rows of the input.}
}
\details{
    \code{text_ntype} counts the number of unique types in each text;
    \code{text_types} returns the set of unique types, as a character
    vector. Types are determined according to the \code{filter} argument.
}
\value{
    If \code{group} is \code{NULL}, the result of \code{text_ntype} is a
    numeric vector with the same length and names as the input text, with the
    elements given the (weighted) number of units in the corresponding text
    element. For \code{text_types}, the result is a list of character vectors
    with each vector giving the unique types in the corresponding text.

    Texts with \code{weights} values of zero get there counts set to zero, if
    even the text value is \code{NA}.  The magnitude and sign of the
    \code{weights} values do not affect the output, only whether the weight is
    nonzero.

    If \code{group} is non-\code{NULL}, then type sets are aggregated by the
    levels of the \code{group} argument, and the length and names of the
    output match the \code{group} levels. For \code{text_ntype}, if
    \code{collapse = TRUE}, then we ignore the \code{group} argument and
    we aggregate over all rows of the input.
}
\seealso{
    \code{\link{token_filter}}, \code{\link{text_length}}.
}
\examples{
    text <- c("I saw Mr. Jones today.",
              "Split across\na line.",
              "What. Are. You. Doing????",
              "She asked 'do you really mean that?' and I said 'yes.'")

    # count the number of unique types
    text_ntype(text)
    text_ntype(text, collapse = TRUE)

    # get the type sets
    text_types(text)
    text_types(text, collapse = TRUE)
}

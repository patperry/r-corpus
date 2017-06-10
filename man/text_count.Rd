\name{text_count}
\alias{text_count}
\title{Counting text units}
\description{
    Count the number of sentences, tokens, or types.
}
\usage{
    text_count(x, units = "tokens",
               filter = if (units == "sentences") sentence_filter()
                        else token_filter(),
               weights = NULL, group = NULL)
}
\arguments{
    \item{x}{a text or character vector.}

    \item{units}{the block size units, either \code{"sentences"},
        \code{"tokens"}, or \code{"types"}.}

    \item{filter}{when \code{units = "sentences"}, a sentence filter
        defining the sentence boundaries; otherwise, a token filter
        defining the token boundaries.}

    \item{weights}{a numeric vector the same length of \code{x} assigning
        weights to each text, or \code{NULL}.}

    \item{group}{if non-\code{NULL}, a factor, character string, or
        integer vector the same length of \code{x} specifying the grouping
        behavior.}
}
\details{
    \code{text_count} counts the number of units in each text element:
    sentences, non-\code{NA} tokens, or unique types, depending on
    the value of the \code{units} argument.

    See \code{\link{sentence_filter}} and \code{\link{token_filter}} for
    the sentence and token definitions.
}
\value{
    If \code{group} is \code{NULL}, the result is a numeric vector with the
    same length and names as the input text, with the elements given the
    (weighted) number of units in the corresponding text element.

    Texts with \code{weights} values of zero get there counts set to
    zero, if even the text value is \code{NA}.

    When \code{units = "types"}, the magnitude and sign of the
    \code{weights} values do not affect the output, only whether
    the weight is nonzero.

    If \code{group} is non-\code{NULL}, then counts are aggregated by
    the levels of the argument, and the length and names of the output
    match the \code{group} levels.
}
\seealso{
    \code{\link{abbreviations}}, \code{\link{sentence_filter}},
        \code{\link{token_filter}}.
}
\examples{
    text <- c("I saw Mr. Jones today.",
              "Split across\na line.",
              "What. Are. You. Doing????",
              "She asked 'do you really mean that?' and I said 'yes.'")

    # count the number of sentences or tokens
    text_count(text, units = "sentences")
    text_count(text, units = "tokens")
}

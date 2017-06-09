\name{text_count}
\alias{text_count}
\title{Counting text units}
\description{
    Count the number of sentences, tokens, or types.
}
\usage{
    text_count(x, units = "tokens",
               filter = if (units == "sentences") sentence_filter()
                        else token_filter())
}
\arguments{
    \item{x}{a text or character vector.}

    \item{units}{the block size units, either \code{"sentences"},
        \code{"tokens"}, or \code{"types"}.}

    \item{filter}{when \code{units = "sentences"}, a sentence filter
        defining the sentence boundaries; otherwise, a token filter
        defining the token boundaries.}
}
\details{
    \code{text_count} counts the number of units in each text element:
    sentences, non-\code{NA} tokens, or unique types, depending on
    the value of the \code{units} argument.

    See \code{\link{sentence_filter}} and \code{\link{token_filter}} for
    the sentence and token definitions.
}
\value{
    A numeric vector with the same length and names as the input text,
    with the elements given the number of units in the corresponding
    text element.
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

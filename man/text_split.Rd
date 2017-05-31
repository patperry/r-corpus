\name{text_split}
\alias{sentences}
\alias{text_split}
\title{Segmenting Text}
\description{
    Segment text into blocks.
}
\usage{
    text_split(x, units = "sentences", size = 1,
               filter = token_filter(), crlf_break = FALSE,
               suppress = abbreviations("english"))
}
\arguments{
    \item{x}{a text or character vector.}

    \item{units}{the block unit, either \code{"sentences"} or \code{"tokens"}.}

    \item{size}{the block size, a positive integer giving the number of
        units per block.}

    \item{filter}{when \code{units = "tokens"}, a text filter defining
        the token boundaries in the text.}

    \item{crlf_break}{when \code{units = "sentences"}, a logical value
        indicating whether to break sentences on carriage returns or
        line feeds.}

    \item{suppress}{when \code{units = "sentences"}, a character vector
        of sentence break suppressions.}
}
\details{
    \code{text_split} splits text into blocks of the given size.

    When \code{units = "sentences"}, blocks are measured in sentences,
    defined according to the boundaries specified by
    \href{http://unicode.org/reports/tr29/#Sentence_Boundaries}{Unicode
        Standard Annex #29, Section 5}. When \code{units = "tokens"},
    blocks are measured in non-NA tokens.

    The UAX 29 boundaries handle Unicode correctly and they give reasonable
    behavior across a variety of languages.

    Unfortunately, default UAX 29 sentence-breaking rules do not handle
    abbreviations correctly. So, for example, without further tailoring to the
    rules, the text \code{"I saw Mr. Jones today."} will get split into two
    sentences.  To get around this, we allow a \code{suppress} argument, a
    list of sentence break suppressions which, when followed by an uppercase
    character, do not signal the end of a sentence.

    Also, the by default, the UAX 29 rules dictate that carriage returns and
    line feeds signal the end of a sentence. This often leads to incorrect
    breaks, so by default we deviate from the UAX 29 rules and we treat
    carriage return and line feed like ordinary spaces. To switch to the
    strict UAX 29 behavior, specify \code{crlf_break = TRUE}.
}
\value{
    A data frame with three columns: \code{parent}, \code{index}, and
    \code{text}, and one row for each text block. The \code{parent} value
    is the integer index of the parent text in \code{x}; the \code{index} value
    is the integer index of the sentence in its parent; the
    \code{text} value is the text of the block, a value of type
    \code{\link{text}}.
}
\seealso{
    \code{\link{abbreviations}}, \code{\link{tokens}}.
}
\examples{
    text_split("I saw Mr. Jones today.")

    text_split(c("What. Are. You. Doing????",
                "She asked 'do you really mean that?' and I said 'yes.'"))
}

\name{segmentation}
\alias{sentences}
\title{Segmenting Text}
\description{
    Segment text into smaller units.
}
\usage{
    sentences(x)
}
\arguments{
    \item{x}{a text or character vector.}
}
\details{
    \code{sentences} splits text at the sentence boundaries defined by
    \href{http://unicode.org/reports/tr29/#Sentence_Boundaries}{Unicode Standard Annex #29, Section 5}.
    These boundaries handle Unicode correctly and they give reasonable
    behavior across a variety of languages. Unfortunately, the UAX 29
    sentence-breaking rules do not handle abbreviations correctly. So, for
    example, the text \code{"I saw Mr. Jones today."} will get split into
    two sentences.

    Future versions of the \code{sentences} function may change to acommodate
    special rules for abbreviations like "Mr.", "Dr.", etc.
}
\value{
    A list of the same length as \code{x}, with the same names. Each list
    item is a \code{\link{text}} vector with the sentences for the
    corresponding element of \code{x}.
}
\seealso{
    \code{\link{tokens}}.
}
\examples{
    sentences("I saw Mr. Jones today.")

    sentences(c("What. Are. You. Doing????",
                "She asked 'do you really mean that?' and I said 'yes.'"))
}

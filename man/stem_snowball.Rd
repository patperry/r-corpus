\name{stem_snowball}
\alias{stem_snowball}
\title{Snowball Stemmer}
\description{
Stem a set of terms using one of the algorithms provided by the
Snowball stemming library.
}
\usage{
stem_snowball(x, algorithm = "en")
}
\arguments{
\item{x}{character vector of terms to stem.}

\item{algorithm}{stemming algorithm; see \sQuote{Details} for the valid
    choices.}
}
\details{
Apply a Snowball stemming algorithm to a vector of input terms, \code{x},
returning the result in a character vector of the same length with the
same names.

The \code{algorithm} argument specifies the stemming algorithm. Valid choices
include the following:
    \code{"ar"} (\code{"arabic"}),
    \code{"da"} (\code{"danish"}),
	\code{"de"} (\code{"german"}),
    \code{"en"} (\code{"english"}),
    \code{"es"} (\code{"spanish"}),
    \code{"fi"} (\code{"finnish"}),
    \code{"fr"} (\code{"french"}),
    \code{"hu"} (\code{"hungarian"}),
    \code{"it"} (\code{"italian"}),
    \code{"nl"} (\code{"dutch"}),
    \code{"no"} (\code{"norwegian"}),
    \code{"pt"} (\code{"portuguese"}),
	\code{"ro"} (\code{"romanian"}),
    \code{"ru"} (\code{"russian"}),
    \code{"sv"} (\code{"swedish"}),
    \code{"ta"} (\code{"tamil"}),
    \code{"tr"} (\code{"turkish"}),
    and \code{"porter"}.
Setting \code{algorithm = NULL} gives a stemmer that returns its input
unchanged.

The function only stems single-word terms of kind "letter"; it leaves
other inputs (multi-word terms, and terms of kind "number", "punct", and
"symbol") unchanged.

The \href{http://snowballstem.org/algorithms/}{Snowball stemming library}
provides the underlying implementation. The \code{wordStem} function from
the \pkg{SnowballC} package provides a similar interface, but that function
applies the algorithm to all input terms, regardless of the kind of the term.
}
\value{
A character vector the same length and names as the input, \code{x}, with
entries containing the corresponding stems.
}
\seealso{
\code{\link{new_stemmer}}, \link{text_filter}.
}
\examples{
# apply english stemming algorithm; don't stem non-letter terms
stem_snowball(c("win", "winning", "winner", "#winning"))

# compare with SnowballC, which stems all kinds, not just letter
\dontrun{SnowballC::wordStem(c("win", "winning", "winner", "#winning"), "en")}
}

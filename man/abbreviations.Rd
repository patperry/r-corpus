\name{abbreviations}
\docType{data}
\alias{abbreviations}
\alias{abbreviations_de}
\alias{abbreviations_en}
\alias{abbreviations_es}
\alias{abbreviations_fr}
\alias{abbreviations_it}
\alias{abbreviations_pt}
\alias{abbreviations_ru}
\title{Abbreviations}
\description{
Lists of common abbreviations.
}
\details{
The \code{abbreviations_} objects are character vectors of abbreviations.
These are words or phrases containing full stops (periods, ambiguous sentence
terminators) that require special handling for sentence detection and
tokenization.

The original lists were compiled by the
\href{http://cldr.unicode.org/}{Unicode Common Locale Data Repository}. We
have tailored the English list by adding single-letter abbreviations and
making a few other additions.

The built-in abbreviation lists are reasonable defaults, but they may require
further tailoring to suit your particular task.
}
\usage{
abbreviations_de
abbreviations_en
abbreviations_es
abbreviations_fr
abbreviations_it
abbreviations_pt
abbreviations_ru
}
\format{A character vector of unique abbreviations.}
\seealso{
\code{\link{text_filter}}.
}
\keyword{datasets}

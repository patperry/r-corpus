\name{stopwords}
\docType{data}
\alias{stopwords}
\alias{stopwords_da}
\alias{stopwords_de}
\alias{stopwords_en}
\alias{stopwords_es}
\alias{stopwords_fi}
\alias{stopwords_fr}
\alias{stopwords_hu}
\alias{stopwords_it}
\alias{stopwords_nl}
\alias{stopwords_no}
\alias{stopwords_pt}
\alias{stopwords_ru}
\alias{stopwords_sv}
\title{Stop Words}
\description{
Lists of common function words (\sQuote{stop} words).
}
\details{
The \code{stopwords_} objects are character vectors of case-folded
\sQuote{stop} words. These are common function words that often get discarded
before performing other text analysis tasks.

There are lists available for the following languages:
Danish (\code{stopwords_da}), Dutch (\code{stopwords_nl}),
English (\code{stopwords_en}), Finnish (\code{stopwords_fi}),
French (\code{stopwords_fr}, German (\code{stopwords_de})
Hungarian (\code{stopwords_hu}), Italian (\code{stopwords_it}),
Norwegian (\code{stopwords_no}), Portuguese (\code{stopwords_pt}),
Russian (\code{stopwords_ru}), Spanish (\code{stopwords_es}),
and Swedish (\code{stopwords_sv}).

These built-in word lists are reasonable defaults, but they may require
further tailoring to suit your particular task. The original lists were
compiled by the \href{http://snowballstem.org/}{Snowball stemming project}.
Following the Quanteda text analysis software, we have tailored the original
lists by adding the word "will" to the English list.
}
\usage{
stopwords_da
stopwords_de
stopwords_en
stopwords_es
stopwords_fi
stopwords_fr
stopwords_hu
stopwords_it
stopwords_nl
stopwords_no
stopwords_pt
stopwords_ru
stopwords_sv
}
\format{A character vector of unique stop words.}
\seealso{
\code{\link{text_filter}}
}
\keyword{datasets}

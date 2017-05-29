\name{corpus-package}
\alias{corpus-package}
\alias{corpus}
\docType{package}
\title{
The Corpus Package
}
\description{
Text corpus analysis functions
}
\details{
This package contains functions for text corpus analysis. To create a
\code{\link{text}} object, use the \code{\link{read_ndjson}} or
\code{\link{as_text}} function. To specify preprocessing behavior for
transforming a text into a token sequence, use the
\code{\link{token_filter}} function. To tokenize text
or compute term frequencies, use the \code{\link{tokens}},
\code{\link{term_counts}}, or \code{\link{term_matrix}} function.

For a complete list of functions, use \code{library(help = "corpus")}.
}
\author{
Patrick O. Perry
}
\keyword{ package }

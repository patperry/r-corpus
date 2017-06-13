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
This package contains functions for text corpus analysis. To create a text
object, use the \code{\link{read_ndjson}} or \code{\link{as_text}} function.
To split text into sentences or token blocks, use the \code{\link{text_split}}
function.  To specify preprocessing behavior for transforming a text into a
token sequence, use the \code{\link{token_filter}} function. To tokenize text
or compute term frequencies, use the \code{\link{text_tokens}},
\code{\link{term_counts}}, \code{\link{term_matrix}} or
\code{\link{term_frame}} function.

For a complete list of functions, use \code{library(help = "corpus")}.
}
\author{
Patrick O. Perry
}
\keyword{ package }

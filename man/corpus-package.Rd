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
object, use the \code{\link{read_ndjson}} or \code{\link{as_corpus_text}}
function.
To split text into sentences or token blocks, use \code{\link{text_split}}.
To specify preprocessing behavior for transforming a text into a
token sequence, use \code{\link{text_filter}}. To tokenize text
or compute term frequencies, use \code{\link{text_tokens}},
\code{\link{term_stats}} or \code{\link{term_matrix}}.
To search for or count specific terms,
use \code{\link{text_locate}}, \code{\link{text_count}}, or
\code{\link{text_detect}}.

For a complete list of functions, use \code{library(help = "corpus")}.
}
\author{
Patrick O. Perry
}
\keyword{ package }

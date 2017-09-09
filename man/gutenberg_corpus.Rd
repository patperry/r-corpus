\name{gutenberg_corpus}
\alias{gutenberg_corpus}
\title{Project Gutenberg Corpora}
\description{
Get a corpus of texts from Project Gutenberg.
}
\usage{
gutenberg_corpus(ids, filter = NULL, mirror = NULL, verbose = TRUE, ...)
}
\arguments{
\item{ids}{an integer vector of requested Gutenberg text IDs.}

\item{filter}{a text filter to set on the corpus.}

\item{mirror}{a character string URL for the Gutenberg mirror to use,
    or NULL to determine automatically.}

\item{verbose}{a logical scalar indicating whether to print progress
    updates to the console.}

\item{...}{additional arguments passed to \code{as_corpus}.}
}
\details{
\code{gutenberg_corpus} downloads a set of texts from Project Gutenberg,
creating a corpus with the texts as rows. You specify the texts for inclusion
using their Project Gutenberg IDs, passed to the function in the
\code{ids} argument.

You can search for Project Gutenberg texts and get their IDs using the
\code{gutenberg_works} function from the \code{gutenbergr} package.
}
\value{
A corpus (data frame) with three columns: \code{"title"}, \code{"author"},
and \code{"text"}.
}
\seealso{
\code{\link{corpus_frame}}.
}
\examples{
# get the texts of George Eliot's novels
\dontrun{eliot <- gutenberg_corpus(c(145, 550, 6688))}
}

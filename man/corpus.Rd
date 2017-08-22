\name{corpus}
\alias{as_corpus}
\alias{as_corpus.character}
\alias{as_corpus.corpus_json}
\alias{as_corpus.corpus_text}
\alias{as_corpus.data.frame}
\alias{as_corpus.default}
\alias{corpus}
\alias{is_corpus}
\title{Corpus Objects}
\description{
    Create or test for corpus objects.
}
\usage{
corpus(..., row.names = NULL, filter = NULL)

as_corpus(x, row.names = NULL, filter = NULL, ...)

is_corpus(x)
}
\arguments{
    \item{\dots}{data frame columns for \code{corpus};
        further arguments passed to or from other methods for
        \code{as_corpus}.}

    \item{row.names}{character vector of row names for the corpus object.}

    \item{filter}{text filter object for the corpus object.}

    \item{x}{object to be coerced or tested.}
}
\details{
    These functions create or convert another object to a corpus object.
    A corpus object is just a data frame with special functions for
    printing, and a column names \code{"text"} of type \code{"corpus_text"}.

    \code{corpus} has similar semantics to the \code{\link{data.frame}}
    function, except that string columns do not get converted to factors.
    
    \code{as_corpus} converts another object to a corpus
    object. By default, the method converts \code{x} to a data frame with
    a column named \code{"text"} of type \code{"corpus_text"}, and sets the
    class attribute of the result to \code{c("corpus_frame", "data.frame")}.

    \code{is_corpus} tests whether \code{x} is a data frame with a column
    named \code{"text"} of type \code{"corpus_text"}.
    
    \code{as_corpus} is generic: you can write methods to
    handle specific classes of objects.
}
\value{
    \code{corpus} creates a data frame with a column named \code{"text"}
        of type \code{"corpus_text"}, and a class attribute set to
        \code{c("corpus_frame", "data.frame")}.

    \code{as_corpus} attempts to coerce its argument to a corpus object,
        setting the \code{row.names} and \code{text_filter} properties.

    \code{is_corpus} returns \code{TRUE} or \code{FALSE} depending on
        whether its argument is a valid corpus object or not.
}
\seealso{
    \code{\link{corpus-package}}, \code{\link{corpus_frame}},
    \code{\link{corpus_text}}, \code{\link{read_ndjson}}.
}
\examples{
    # convert a data frame:
    emoji <- data.frame(text = sapply(0x1f600 + 1:30, intToUtf8),
                        stringsAsFactors = FALSE)
    as_corpus(emoji)

    # construct directly (no need for stringsAsFactors = FALSE):
    corpus(text = sapply(0x1f600 + 1:30, intToUtf8))
    
    # convert a character vector:
    as_corpus(c(a = "goodnight", b = "moon")) # keeps names
    as_corpus(c(a = "goodnight", b = "moon"), row.names = NULL) # drops names
}
\keyword{classes}

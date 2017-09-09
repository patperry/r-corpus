\name{corpus_frame}
\alias{as_corpus_frame}
\alias{as_corpus_frame.character}
\alias{as_corpus_frame.Corpus}
\alias{as_corpus_frame.corpus}
\alias{as_corpus_frame.corpus_json}
\alias{as_corpus_frame.corpus_text}
\alias{as_corpus_frame.data.frame}
\alias{as_corpus_frame.default}
\alias{corpus_frame}
\alias{is_corpus_frame}
\title{Corpus Data Frame}
\description{
Create or test for corpus objects.
}
\usage{
corpus_frame(..., row.names = NULL, filter = NULL)

as_corpus_frame(x, filter = NULL, ..., row.names = NULL)

is_corpus_frame(x)
}
\arguments{
\item{\dots}{data frame columns for \code{corpus_frame};
    further arguments passed to \code{as_corpus_text} from
    \code{as_corpus_frame}.}

\item{row.names}{character vector of row names for the corpus object.}

\item{filter}{text filter object for the \code{"text"} column in the
    corpus object.}

\item{x}{object to be coerced or tested.}
}
\details{
    These functions create or convert another object to a corpus object.
    A corpus object is just a data frame with special functions for
    printing, and a column names \code{"text"} of type \code{"corpus_text"}.

    \code{corpus} has similar semantics to the \code{\link{data.frame}}
    function, except that string columns do not get converted to factors.
    
    \code{as_corpus_frame} converts another object to a corpus data frame
    object. By default, the method converts \code{x} to a data frame with
    a column named \code{"text"} of type \code{"corpus_text"}, and sets the
    class attribute of the result to \code{c("corpus_frame", "data.frame")}.

    \code{is_corpus_frame} tests whether \code{x} is a data frame with a column
    named \code{"text"} of type \code{"corpus_text"}.
    
    \code{as_corpus_frame} is generic: you can write methods to
    handle specific classes of objects.
}
\value{
    \code{corpus_frame} creates a data frame with a column named \code{"text"}
        of type \code{"corpus_text"}, and a class attribute set to
        \code{c("corpus_frame", "data.frame")}.

    \code{as_corpus_frame} attempts to coerce its argument to a corpus
        data frame object, setting the \code{row.names} and calling
        \code{\link{as_corpus_text}} on the \code{"text"} column with
        the \code{filter} and \code{\dots} arguments.

    \code{is_corpus_frame} returns \code{TRUE} or \code{FALSE} depending on
        whether its argument is a valid corpus object or not.
}
\seealso{
\code{\link{corpus-package}}, \code{\link{print.corpus_frame}},
\code{\link{corpus_text}}, \code{\link{read_ndjson}}.
}
\examples{
# convert a data frame:
emoji <- data.frame(text = sapply(0x1f600 + 1:30, intToUtf8),
                    stringsAsFactors = FALSE)
as_corpus_frame(emoji)

# construct directly (no need for stringsAsFactors = FALSE):
corpus_frame(text = sapply(0x1f600 + 1:30, intToUtf8))
    
# convert a character vector:
as_corpus_frame(c(a = "goodnight", b = "moon")) # keeps names
as_corpus_frame(c(a = "goodnight", b = "moon"), row.names = NULL) # drops names
}
\keyword{classes}

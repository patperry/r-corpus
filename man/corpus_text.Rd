\name{corpus_text}
\alias{as_corpus_text}
\alias{as_corpus_text.character}
\alias{as_corpus_text.Corpus}
\alias{as_corpus_text.corpus}
\alias{as_corpus_text.corpus_json}
\alias{as_corpus_text.corpus_text}
\alias{as_corpus_text.data.frame}
\alias{as_corpus_text.default}
\alias{corpus_text}
\alias{is_corpus_text}
\title{Text Objects}
\description{
Create or test for text objects.
}
\usage{
as_corpus_text(x, filter = NULL, ..., names = NULL)

is_corpus_text(x)
}
\arguments{
  \item{x}{object to be coerced or tested.}

  \item{filter}{if non-\code{NULL}, a text filter for the converted result.}

  \item{\dots}{text filter properties to set on the result.}

  \item{names}{if non-\code{NULL} character vector of names for
    the converted result.}
}
\details{
The \code{corpus_text} type is a new data type provided by the \code{corpus}
package suitable for processing international (Unicode) text. Text vectors
behave like character vectors (and can be converted to them with the
\code{as.character} function). They can be created using the
\code{\link{read_ndjson}} function or by converting another object using the
\code{as_corpus_text} function.

All text objects have a \code{\link{text_filter}} property specify how to
transform the text into tokens or segment it into sentences.

The default behavior for \code{as_corpus_text} is to proceed as follows:
\enumerate{
  \item If \code{x} is a \code{character} vector, then we create
    a new \code{text} vector from \code{x}.

   \item If \code{x} is a data frame, then we call \code{as_corpus_text}
     on \code{x$text} if a column named \code{"text"} exists in
     the data frame.  If the data frame does not have a column
     named \code{"text"}, then we fail with an error message.

   \item If \code{x} is a \code{corpus_text} object, then we drop all
     attributes and we set the class to \code{"corpus_text"}.

   \item The default behavior for when none of the above conditions
     are true is to call \code{as.character} on the object first,
     preserving the names, and then and call \code{as_corpus_text} on
     the returned character object.
}

In all cases, when the \code{names} is \code{NULL}, we set the result
names to \code{names(x)} (or \code{rownames(x)} for a data frame
argument). When \code{names} is a character vector, we set the result names
to this vector of names

Similarly, when \code{filter} is \code{NULL}, we set the result text
filter to \code{text_filter(x)}. When \code{filter} is non-\code{NULL}
missing, we set the result text filter to this value. In either case,
if there are additional names arguments, then we override the filter
properties specified by the names of these arguments with the new values
given.

Note that the special handling for the names of the object is different
from the other R conversion functions (\code{as.numeric},
\code{as.character}, etc.), which drop the names.

\code{as_corpus_text} is generic: you can write methods to handle specific
classes of objects.
}
\value{
\code{as_corpus_text} attempts to coerce its argument to \code{text} type and
  set its \code{names} and \code{text_filter} properties; it strips
  all other attributes.

\code{is_corpus_text} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of text type or not.
}
\seealso{
\code{\link{as_utf8}}, \code{\link{text_filter}}, \code{\link{read_ndjson}}.
}
\examples{
as_corpus_text("hello, world!")
as_corpus_text(c(a = "goodnight", b = "moon")) # keeps names

# set a filter property
as_corpus_text(c(a = "goodnight", b = "moon"), stemmer = "english") 

is_corpus_text("hello") # FALSE, "hello" is character, not text
}
\keyword{classes}

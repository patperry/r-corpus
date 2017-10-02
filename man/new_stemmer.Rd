\name{new_stemmer}
\alias{new_stemmer}
\title{Stemmer Construction}
\description{
    Make a stemmer from a set of (term, stem) pairs.
}
\usage{
new_stemmer(term, stem, default = NULL, duplicates = "first",
            vectorize = TRUE)
}
\arguments{
\item{term}{character vector of terms to stem.}

\item{stem}{character vector the same length as \code{term} with entries
   giving the corresponding stems.}

\item{default}{if non-\code{NULL}, a default value to use for terms
    that do not have a stem; \code{NULL} specifies that such terms
    should be left unchanged.}

\item{duplicates}{action to take for duplicates in the \code{term} list. See
    \sQuote{Details}}.

\item{vectorize}{whether to produce a vectorized stemmer that accepts and
    returns vector arguments.}
}
\details{
Giving a list of terms and a corresponding list of stems, this produces a
function that maps terms to their corresponding entry. If
\code{default = NULL}, then values absent from the \code{term} argument
get left as-is; otherwise, they get replaced by the \code{default} value.

The \code{duplicates} argument indicates the action to take if
there are duplicate entries in the \code{term} argument:
\itemize{
  \item \code{duplicates = "first"} take the first matching entry in the
    \code{stem} list.

  \item \code{duplicates = "last"} take the last matching entry in the
    \code{stem} list.

  \item \code{duplicates = "omit"} use the \code{default} value for
    duplicated terms.

  \item \code{duplicates = "fail"} raise an error if there are duplicated
    terms.
}
}
\value{
By default, with \code{vectorize = TRUE}, the resulting stemmer accepts a
character vector as input and returns a character vector of the same length
with entries giving the stems of the corresponding input entries.

Setting \code{vectorize = FALSE} gives a function that accepts a single input
and returns a single output. This can be more efficient when used as part of
a \code{\link{text_filter}}.
}
\seealso{
\code{\link{stem_snowball}, \link{text_filter}}, \code{\link{text_tokens}}.
}
\examples{
# map uppercase to lowercase, leave others unchanged
stemmer <- new_stemmer(LETTERS, letters)
stemmer(c("A", "E", "I", "O", "U", "1", "2", "3"))

# map uppercase to lowercase, drop others
stemmer <- new_stemmer(LETTERS, letters, default = NA)
stemmer(c("A", "E", "I", "O", "U", "1", "2", "3"))
}

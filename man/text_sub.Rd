\name{text_sub}
\alias{text_sub}
\title{Text Subsequences}
\description{
    Extract token subsequences from a set of texts.
}
\usage{
text_sub(x, start = 1L, end = -1L, filter = NULL, ...)
}
\arguments{
\item{x}{text vector or corpus object.}

\item{start}{integer vector giving the starting positions of the
    subsequences, or a two-column integer matrix giving the starting
    and ending positions.}

\item{end}{integer vector giving the ending positions of the
    subsequences; ignored if \code{start} is a two-column matrix.}

\item{filter}{if non-\code{NULL}, a text filter to to use instead of
    the default text filter for \code{x}.}

\item{\dots}{additional properties to set on the text filter.}
}
\details{
    \code{text_sub} extracts token subsequences from a set of texts.
    The \code{start} and \code{end} arguments specifying the
    positions of the subsequences within the parent texts, as an inclusive
    range.  Negative indices are interpreted as counting from the end of
    the text, with \code{-1L} referring to the last element.
}
\value{
    A text vector with the same length and names as \code{x}, with the
    desired subsequences.
}
\seealso{
    \code{\link{text_tokens}}, \code{\link{text_ntoken}}.
}
\examples{
x <- as_corpus_text(c("A man, a plan.", "A \"canal\"?", "Panama!"),
                    drop_punct = TRUE)

# entire text
text_sub(x, 1, -1)

# first three elements
text_sub(x, 1, 3)

# last two elements
text_sub(x, -2, -1)
}

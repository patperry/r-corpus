\name{text_locate}
\alias{text_count}
\alias{text_detect}
\alias{text_locate}
\title{Searching for terms in text.}
\description{
    Look for instances of one or more terms in a set of texts.
}
\usage{
    text_count(x, terms, filter = text_filter(x))

    text_detect(x, terms, filter = text_filter(x))
    
    text_locate(x, terms, filter = text_filter(x))

    text_subset(x, terms, filter = text_filter(x))
}
\arguments{
    \item{x}{a text or character vector.}

    \item{terms}{a character vector of search terms.}

    \item{filter}{a text filter defining the token boundaries.}

}
\details{
    \code{text_count} counts the number of search term instances in
    each element of the text vector.

    \code{text_detect} indicates whether each text contains at least
    one of the search terms.

    \code{text_locate} finds all instances of the search terms in the
    input text.

    \code{text_subset} returns the texts that contain the search terms.
}
\value{
    \code{text_count} and \code{text_detect} return a numeric vector and
    a logical vector, respectively, with length equal to the number of input
    texts and names equal to the text names.

    \code{text_locate} returns a data frame with one row for each search
    result and columns named \sQuote{text}, \sQuote{term}, \sQuote{before},
    \sQuote{instance}, and \sQuote{after}. The \sQuote{text} column gives
    the name of the text containing the instance, and \sQuote{term} gives
    the matching term; \sQuote{before} and \sQuote{after} are text vectors
    giving the text before and after the instance. The \sQuote{instance}
    column gives the token or tokens matching the search term.

    \code{text_subset} returns the subset of texts that contain the given
    search terms.  The resulting has its \code{text_filter} set to the
    passed-in \code{filter} argument.
}
\seealso{
    \code{\link{term_counts}}, \code{\link{term_frame}}.
}
\examples{
    text <- c("Rose is a rose is a rose is a rose.",
              "A rose by any other name would smell as sweet.",
              "Snow White and Rose Red")

    text_count(text, "rose")
    text_detect(text, "rose")
    text_locate(text, "rose")
    text_subset(text, "a rose")

    # search for multiple terms
    text_locate(text, c("rose", "rose red", "snow white"))
}

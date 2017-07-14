\name{stopwords}
\alias{stopwords}
\title{Stop Words}
\description{
    Get a list of common function words (\sQuote{stop} words).
}
\usage{
    stopwords(kind = "english")
}
\arguments{
    \item{kind}{a character vector giving the desired name or names
        of the stop word list(s), \code{NA}, or \code{NULL}. Allowed
        values are \code{"danish"}, \code{"dutch"}, \code{"english"},
        \code{"finnish"}, \code{"french"}, \code{"german"},
        \code{"hungarian"}, \code{"italian"}, \code{"norwegian"},
        \code{"portuguese"}, and \code{"russian"}; these values
        retrieve the language-specific stop word lists compiled by the
        \href{http://snowballstem.org/}{Snowball stemming project}.}
}
\details{
    \code{stopwords} returns a character vector of case-folded \sQuote{stop}
    words. These are common function words that often get discarded before
    performing other text analysis tasks.

    The built-in word lists returned by this function are reasonable defaults,
    but they may require further tailoring to suit your particular task.
}
\value{
    A character vector of unique stop words of the specified kind (or kinds
    if \code{kind} is a vector), or \code{NULL} if \code{kind = NULL}.
}
\seealso{
    \code{\link{text_filter}}
}
\examples{
    head(stopwords("english"))
    head(stopwords("russian"))
    stopwords(NULL)

    # combine multiple lists, removing duplicates
    head(stopwords(c("spanish", "portuguese")))

    # add words to the default list:
    my_stopwords <- c(stopwords("english"), "will", "mr", "mrs", "ms")
}

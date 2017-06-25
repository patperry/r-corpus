\name{federalist}
\docType{data}
\alias{federalist}
\title{The Federalist Papers}
\description{
    \cite{The Federalist Papers} comprise 85 articles published under the
    pseudonym \dQuote{Publius} in New York newspapers between 1787 and
    1788, written to convince residents to ratify the \cite{Constitution}.
    John Jay wrote 5 papers, while Alexander Hamilton and James Madison
    wrote the remaining 80.  Between the last two authors there are
    conflicting accounts of which author wrote which paper.  Most sources
    agree on the authorships of 65 papers (51 by Hamilton and 14 by Madison),
    but 15 papers are in dispute.

    In one of the earliest examples of statistical text analysis, F. Mosteller
    and D. L. Wallace used a form of Naive Bayes classification to identify
    the authorships of the 15 disputed papers, finding strong evidence that
    Madison was the author of all of the disputed papers.
}
\usage{federalist}
\format{A data frame with 85 rows, one for each paper.}
\source{\url{http://www.gutenberg.org/ebooks/18}}
\references{
    Mosteller, F and Wallace, D. L. (1963).
    Inference in an authorship problem.
    \emph{Journal of the American Statistical Association}
    \strong{58} 275--309.
}
\keyword{datasets}

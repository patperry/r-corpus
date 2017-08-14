\name{wnaffect}
\docType{data}
\alias{wnaffect}
\title{WordNet-Affect Lexicon}
\description{
    The WordNet-Affect Lexicon is a hand-curate collection of
    emotion-related words (nouns, verbs, adjectives, and adverbs),
    classified as \dQuote{Positive}, \dQuote{Negative}, and
    \dQuote{Neutral}, and categorized into 29 subcategories
    (\dQuote{Joy}, \dQuote{Love}, \dQuote{Fear}, etc.).

    Terms can and do appear in multiple categories.

    The original lexicon uses four main subcategories, but as
    presented here, the lexicon combines \dQuote{neutral}
    and \dQuote{ambiguous} emotions into a single class. Also,
    the original lexicon contains multi-word phrases, but they
    are excluded here.
}
\usage{wnaffect}
\format{A data frame with one row for each term classification.}
\source{\url{http://wndomains.fbk.eu/wnaffect.html}}
\references{
    Strapparava, C and Valitutti A. (2004).
    Wordnet-affect: an affective extension of wordnet.
    \emph{Proceedings of the 4th International Conference on Language
        Resources and Evaluation}
    1083--1086.
}
\keyword{datasets}

\name{wnaffect}
\docType{data}
\alias{wnaffect}
\title{WordNet-Affect Lexicon}
\description{
    The WordNet-Affect Lexicon is a hand-curate collection of
    emotion-related words (nouns, verbs, adjectives, and adverbs),
    classified as \dQuote{Positive}, \dQuote{Negative},
    \dQuote{Neutral}, or \dQuote{Ambiguous} and categorized into
    28 subcategories (\dQuote{Joy}, \dQuote{Love}, \dQuote{Fear},
    etc.).

    Terms can and do appear in multiple categories.

    The original lexicon contains multi-word phrases, but they
    are excluded here. Also, we removed the term \sQuote{thing}
    from the lexicon.
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

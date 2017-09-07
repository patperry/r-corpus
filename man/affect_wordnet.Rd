\name{affect_wordnet}
\docType{data}
\alias{affect_wordnet}
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

    The original WordNet-Affect lexicon is distributed as part
    of the WordNet Domains project, which is licensed under a
    \href{https://creativecommons.org/licenses/by/3.0/}{Creative Commons Attribution 3.0 Unported License}.
    You are free to share and adapt the lexicon, as long as you
    give attribution to the original authors.
}
\usage{affect_wordnet}
\format{A data frame with one row for each term classification.}
\source{\url{http://wndomains.fbk.eu/wnaffect.html}}
\references{
    Strapparava, C and Valitutti A. (2004).
    WordNet-Affect: an affective extension of WordNet.
    \emph{Proceedings of the 4th International Conference on Language
        Resources and Evaluation}
    1083--1086.
}
\keyword{datasets}

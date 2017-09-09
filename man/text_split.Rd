\name{text_split}
\alias{sentences}
\alias{text_nsentence}
\alias{text_split}
\title{Segmenting Text}
\description{
    Segment text into smaller units.
}
\usage{
text_split(x, units = "sentences", size = 1, filter = NULL, ...)

text_nsentence(x, filter = NULL, ...)
}
\arguments{
\item{x}{a text or character vector.}

\item{units}{the block size units, either \code{"sentences"} or
    \code{"tokens"}.}

\item{size}{the block size, a positive integer giving the maximum
    number of units per block.}

\item{filter}{if non-\code{NULL}, a text filter to to use instead of
    the default text filter for \code{x}.}

\item{\dots}{additional properties to set on the text filter.}
}
\details{
    \code{text_split} splits text into roughly evenly-sized blocks,
    measured in the specified units.  When \code{units = "sentences"},
    units are sentences; when \code{units = "tokens"}, units are
    non-\code{NA} tokens. The \code{size} parameter specifies the
    maximum block size.

    When the minimum block size does not evenly divide the number of
    total units in a text, the block sizes will not be exactly equal.
    However, it will still be the case that no block will has more
    than one unit more than any other block. The extra units get
    allocated to the first segments in the split.

    Sentences and tokens are defined by the \code{filter} argument.
    The documentation for \code{\link{text_tokens}} describes the
    tokenization rules. For sentence boundaries, see the
    \sQuote{Sentences} section below.
}
\section{Sentences}{
    Sentences are defined according to a tailored version of the
    boundaries specified by
    \href{http://unicode.org/reports/tr29/#Sentence_Boundaries}{Unicode
        Standard Annex #29, Section 5}.

    The UAX 29 sentence boundaries handle Unicode correctly and they give
    reasonable behavior across a variety of languages, but they do not
    handle abbreviations correctly and by default they treat carriage
    returns and line feeds as paragraph separators, often leading to
    incorrect breaks. To get around these shortcomings, the
    text filter allows tailoring the UAX 29 rules using the
    \code{sent_crlf} and the \code{sent_suppress} properties.

    The UAX 29 rules break after full stops (periods) whenever they are
    followed by uppercase letters. Under these rules, the text
    \code{"I saw Mr. Jones today."} gets split into two sentences. To get
    around this, we allow a \code{sent_suppress} property, a list of sentence
    break suppressions which, when followed by uppercase characters, do
    not signal the end of a sentence.

    The UAX 29 rules also specify that a carriage return (CR) or line
    feed (LF) indicates the end of of a sentence, so that
    \code{"A split\nsentence."} gets split into two sentences. This often
    leads to incorrect breaks, so by default, with \code{sent_crlf = FALSE},
    we deviate from the UAX 29 rules and we treat CR and LF like spaces.
    To break sentences on CRLF, CR, and LF, specify \code{sent_crlf = TRUE}.
}
\value{
    \code{text_split} returns a data frame with three columns named
    \code{parent}, \code{index}, and \code{text}, and one row for each
    text block. The columns are as follows:

    \enumerate{
        \item The \code{parent} column is a factor. The levels of this
          factor are the names of \code{as_corpus_text(x)}. Calling
          \code{as.integer} on the parent column gives the indices of
          the parent texts for the parent text for each sentence.

        \item The \code{index} column gives the integer index of the
            sentence in its parent.

        \item The \code{text} value is the text of the block, a value of
            type \code{\link{corpus_text}} (not a character vector).
    }

    \code{text_nsentence} returns a numeric vector with the same length
    as \code{x} with each element giving the number of sentences in the
    corresponding text.
}
\seealso{
    \code{\link{text_tokens}}, \code{\link{text_filter}}.
}
\examples{
text <- c("I saw Mr. Jones today.", 
          "Split across\na line.",
          "What. Are. You. Doing????",
          "She asked 'do you really mean that?' and I said 'yes.'")

# split text into sentences
text_split(text, units = "sentences")

# get the number of sentences
text_nsentence(text)

# disable the default sentence suppressions
text_split("I saw Mr. Jones today.", units = "sentences", filter = NULL)

# break on CR and LF
text_split("Split across\na line.", units = "sentences",
           filter = text_filter(sent_crlf = TRUE))

# 2-sentence blocks
text_split(c("What. Are. You. Doing????",
           "She asked 'do you really mean that?' and I said 'yes.'"),
           units = "sentences", size = 2)

# 4-token blocks
text_split(c("What. Are. You. Doing????",
             "She asked 'do you really mean that?' and I said 'yes.'"),
           units = "tokens", size = 4)

# blocks are approximately evenly sized; 'size' gives maximum size
text_split(paste(letters, collapse = " "), "tokens", 4)
text_split(paste(letters, collapse = " "), "tokens", 16)
}

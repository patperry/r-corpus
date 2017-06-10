\name{text_split}
\alias{corpus_sentence_filter}
\alias{sentences}
\alias{sentence_filter}
\alias{text_split}
\title{Segmenting Text}
\description{
    Segment text into smaller units.
}
\usage{
    text_split(x, units, size = 1,
               filter = if (units == "sentences") sentence_filter()
                        else token_filter())

    sentence_filter(crlf_break = FALSE,
                    suppress = abbreviations("english"))
}
\arguments{
    \item{x}{a text or character vector.}

    \item{units}{the block size units, either \code{"sentences"} or
        \code{"tokens"}.}

    \item{size}{the block size, a positive integer giving the number of
        units per block.}

    \item{filter}{when \code{units = "sentences"}, a sentence filter
        defining the sentence boundaries; otherwise, a token filter
        defining the token boundaries.}

    \item{crlf_break}{a logical value indicating whether to break
        sentences on carriage returns or line feeds.}

    \item{suppress}{a character vector of sentence break suppressions.}
}
\details{
    \code{text_split} splits text into blocks of the given size, measured
    in the specified units.

    When \code{units = "sentences"}, units are sentences as defined in
    the \sQuote{Sentences} section below.  When \code{units = "tokens"},
    units are non-\code{NA} tokens.
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
    \code{filter} argument, specified using the \code{sentence_filter}
    function, allows tailoring the UAX 29 rules using the
    \code{crlf_break} and the \code{suppress} properties.

    The UAX 29 rules break after full stops (periods) whenever they are
    followed by uppercase letters. Under these rules, the text
    \code{"I saw Mr. Jones today."} gets split into two sentences. To get
    around this, we allow a \code{suppress} property, a list of sentence
    break suppressions which, when followed by uppercase characters, do
    not signal the end of a sentence.

    The UAX 29 rules also specify that a carriage return (CR) or line
    feed (LF) indicates the end of of a sentence, so that
    \code{"A split\nsentence."} gets split into two sentences. This often
    leads to incorrect breaks, so by default, with \code{crlf_break = FALSE},
    we deviate from the UAX 29 rules and we treat CR and LF like spaces.
    To break sentences on CRLF, CR, and LF, specify \code{crlf_break = TRUE}.
}
\value{
    The result is a data frame with three columns named
    \code{parent}, \code{index}, and \code{text}, and one row for each
    text block. The columns are as follows:

    \enumerate{
        \item If \code{as_text(x)} has names, then the
            \code{parent} column is a character vector giving the name of
            the parent text for each sentences. Otherwise, if
            \code{as_text(x)} does not have names, then the \code{parent}
            value is the integer index of the parent text.

        \item The \code{index} column gives the integer index of the
            sentence in its parent.

        \item The \code{text} value is the text of the block, a value of
            type \code{\link{corpus_text}} (not a character vector).
    }
}
\seealso{
    \code{\link{abbreviations}}, \code{\link{text_count}},
        \code{\link{tokens}}.
}
\examples{
    text <- c("I saw Mr. Jones today.", 
              "Split across\na line.",
              "What. Are. You. Doing????",
              "She asked 'do you really mean that?' and I said 'yes.'")

    # split text into sentences
    text_split(text, units = "sentences")

    # disable the default sentence suppressions
    text_split("I saw Mr. Jones today.", units = "sentences", filter = NULL)

    # break on CR and LF
    text_split("Split across\na line.", units = "sentences",
               filter = sentence_filter(crlf_break = TRUE))

    # 2-sentence blocks
    text_split(c("What. Are. You. Doing????",
                "She asked 'do you really mean that?' and I said 'yes.'"),
               units = "sentences", size = 2)

    # 4-token blocks
    text_split(c("What. Are. You. Doing????",
                "She asked 'do you really mean that?' and I said 'yes.'"),
               units = "tokens", size = 4)
}

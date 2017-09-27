\name{text_tokens}
\alias{text_ntoken}
\alias{text_tokens}
\title{Text Tokenization}
\description{
Segment text into tokens, each of which is an instance of a particular
\sQuote{type}.
}
\usage{
text_tokens(x, filter = NULL, ...)

text_ntoken(x, filter = NULL, ...)
}
\arguments{
\item{x}{object to be tokenized.}

\item{filter}{if non-\code{NULL}, a text filter to to use instead of
    the default text filter for \code{x}.}

\item{\dots}{additional properties to set on the text filter.}
}
\details{
\code{text_tokens} splits texts into token sequences. Each token is an
instance of a particular type. This operation proceeds in a series
of stages, controlled by the \code{filter} argument:

\enumerate{
  \item First, we segment the text into words and spaces using the boundaries
    defined by
    \href{http://unicode.org/reports/tr29/#Word_Boundaries}{Unicode Standard Annex #29, Section 4},
    with special handling for @mentions, #hashtags, and URLs.

  \item Next, we normalize the words by applying the character mappings
    indicated by the \code{map_case}, \code{map_quote}, and
    \code{remove_ignorable} properties. We replace sequences of spaces
    by a space (U+0020). At the end of the second stage,
    we have segmented the text into a sequence of normalized words and
    spaces, in Unicode composed normal form (NFC).

  \item In the third stage, if the \code{combine} property is non-\code{NULL},
    we scan the word sequence from left to right, searching for the longest
    possible match in the \code{combine} list. If a match exists, we
    replace the word sequence with a single token for that term;
    otherwise, we leave the word as-is. We drop spaces at this point, unless
    they are part of a multi-word term.  See the \sQuote{Combining words}
    section below for more details.

  \item Next, if the \code{stemmer} property is non-\code{NULL}, we apply
    the indicated stemming algorithm to each word that does not match
    one of the elements of the \code{stem_except} character vector. Terms
    that stem to \code{NA} get dropped from the sequence.

  \item After stemming, we categorize each remaining token as
    \code{"letter"}, \code{"number"}, \code{"punct"}, or \code{"symbol"}
    according to the first character in the word. For words that start with
    extenders like underscore (\code{_}), use the first non-extender to
    classify it.

  \item If any of \code{drop_letter}, \code{drop_number}, \code{drop_punct},
    or \code{drop_symbol} are \code{TRUE}, then we drop the tokens in the
    corresponding categories. We also drop any terms that match an element
    of the \code{drop} character vector.  We can add exceptions to the
    drop rules by specifying a non-\code{NULL} value for the
    \code{drop_except} property: \code{drop_except} is a character
    vector, then we we restore tokens that match elements of vector to
    their values prior to dropping.

  \item Finally, we replace sequences of white-space in the terms with
    the specified \code{connector}, which defaults to a low line character
    (\code{_}, U+005F).
}
Multi-word terms specified by the \code{combine} property can be specified as
tokens, prior to normalization.  Terms specified by the \code{stem_except},
\code{drop}, and \code{drop_except} need to be normalized and stemmed (if
\code{stemmer} is non-\code{NULL}).  Thus, for example, if
\code{map_case = TRUE}, then a token filter with \code{combine = "Mx."}
produces the same results as a token filter with \code{combine = "mx."}.
However, \code{drop = "Mx."} behaves different from \code{drop = "mx."}.
}
\section{Combining words}{
The \code{combine} property of a \code{text_filter} enables
transformations that combine two or more words into a single token. For
example, specifying \code{combine = "new york"} will
cause consecutive instances of the words \code{new} and \code{york}
to get replaced by a single token, \code{new york}.
}
\value{
\code{text_tokens} returns a list of the same length as \code{x}, with
the same names. Each list item is a character vector with the tokens
for the corresponding element of \code{x}.

\code{text_ntoken} returns a numeric vector the same length as \code{x},
with each element giving the number of tokens in the corresponding text.
}
\seealso{
\code{\link{stopwords}}, \code{\link{text_filter}},
\code{\link{text_types}}.
}
\examples{
text_tokens("The quick ('brown') fox can't jump 32.3 feet, right?")

# count tokens:
text_ntoken("The quick ('brown') fox can't jump 32.3 feet, right?")

# don't change case or quotes:
f <- text_filter(map_case = FALSE, map_quote = FALSE)
text_tokens("The quick ('brown') fox can't jump 32.3 feet, right?", f)

# drop common function words ('stop' words):
text_tokens("Able was I ere I saw Elba.",
            text_filter(drop = stopwords_en))

# drop numbers, with some exceptions:"
text_tokens("0, 1, 2, 3, 4, 5",
            text_filter(drop_number = TRUE,
                        drop_except = c("0", "2", "4")))

# apply stemming...
text_tokens("Mary is running", text_filter(stemmer = "english"))

# ...except for certain words
text_tokens("Mary is running",
            text_filter(stemmer = "english", stem_except = "mary"))

# default tokenization
text_tokens("Ms. Jones")

# combine abbreviations
text_tokens("Ms. Jones", text_filter(combine = abbreviations_en))

# add custom combinations
text_tokens("Ms. Jones is from New York City, New York.",
            text_filter(combine = c(abbreviations_en,
                                    "new york", "new york city")))
}

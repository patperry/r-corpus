\name{text_tokens}
\alias{text_ntoken}
\alias{text_tokens}
\title{Text Tokenization}
\description{
   Segment text into tokens, each of which is an instance of
   a particular \sQuote{type}.
}
\usage{
    text_tokens(x, filter = text_filter(x))

    text_ntoken(x, filter = text_filter(x))
}
\arguments{
    \item{x}{object to be tokenized.}

    \item{filter}{filter specifying the transformation from text to
        token sequence.}
}
\details{
    \code{text_tokens} splits texts into token sequences. Each token is an
    instance of a particular type. This operation proceeds in a series
    of stages, controlled by the \code{filter} argument:

    \enumerate{
        \item First, we segment the text into words using the boundaries
            defined by
            \href{http://unicode.org/reports/tr29/#Word_Boundaries}{Unicode
                Standard Annex #29, Section 4}. We categorize each word as
            \code{"letter"}, \code{"mark"}, \code{"number"},
            \code{"punct"}, \code{"symbol"}, \code{"other"} or
            \code{"url"} according to the first character in the word.
            For words with two or more characters that start with extenders
            like underscore (\code{_}), we use the second character in
            the word to categorize it, treating a second extender as a letter.

        \item Next, we normalize the remaining words by applying the
            character mappings indicated by the \code{map_case},
            \code{map_compat}, \code{map_quote}, and \code{remove_ignorable}.
            At the end of the second stage, we have segmented
            the text into a sequence of normalized words, in Unicode composed
            normal form (NFC, or if \code{map_compat} is \code{TRUE},
            NFKC).

        \item In the third stage, if the \code{stemmer} property is
            non-\code{NULL}, we apply the indicated stemming algorithm to
            each word that does not match one of the elements of the
            \code{stem_except} character vector. See the
            \sQuote{Stemming} section below for more information.

        \item Next, if the \code{combine} property is non-\code{NULL},
            we scan the word sequence from left to right, searching for
            the longest possible match in the \code{combine} list. If
            a match exists, we replace the word sequence with a single token
            for that type; otherwise, we create a single-word token. See the
            \sQuote{Combining words} section below for more details.  After
            this stage, the sequence elements are \sQuote{tokens}, not
            \sQuote{words}.

        \item If any of \code{drop_letter}, \code{drop_mark},
            \code{drop_number}, \code{drop_punct}, \code{drop_symbol},
            \code{drop_other}, or \code{drop_url} are \code{TRUE}, we replace
            the tokens with values in the corresponding categories by \code{NA}.
            (For multi-word types created by the \code{combine} step,
            we take the category of the first word in the phrase.)
            Then, if the \code{drop} property is non-\code{NULL}, we replace
            tokens that match elements of this character vector with
            \code{NA}. We can add exceptions to the drop rules by specifying
            a non-\code{NULL} value for the \code{drop_except} property:
            \code{drop_except} is a character vector, then we we restore
            tokens that match elements of vector to their values prior to
            dropping.
    }

    When \code{filter = NULL}, we treat all logical properties as
    \code{FALSE} and all other properties as \code{NA} or \code{NULL}.

    Terms specified by the \code{stem_except}, \code{combine},
    \code{drop}, and \code{drop_except} need to be stemmed (unless
    \code{stemmer} is not \code{NA}), but they do not need to be
    normalized. We normalize the argument values in the manner specified
    by \code{map_case}, \code{map_compate}, \code{map_quote}, and
    \code{remove_ignorable}. Thus, for example, if \code{map_case = TRUE},
    then a token filter with \code{combine = "Mx."} produces the same
    results as a token filter with \code{combine = "mx."}.
}
\section{Stemming}{
    We use the stemming algorithms provided by the Snowball library. These
    algorithms are also available in the \code{SnowballC} R package. Unlike
    that package, we provide the ability to exempt certain words from stemming,
    using the \code{stem_except} argument; see the examples below.
    If you do not specify the \code{stem_except} argument, then we set its
    value equal to the \code{drop} argument.

    We also exempt from stemming any case that would turn internal
    punctuation like the full stop in \code{"u.s"} into boundary punctuation
    like at the end of \code{"u."}; otherwise, in examples like this, the
    stemming procedure would turn single-word tokens into multi-word
    tokens (compare \code{text_tokens("u.s")} with \code{text_tokens("u.")}).
    For English, this likely only affects words ending in \code{".s"}.
}
\section{Combining words}{
    The \code{combine} property of a \code{text_filter} enables
    transformations that combine two or more words into a single token. For
    example, specifying \code{combine = "new york"} will
    cause consecutive instances of the words \code{new} and \code{york}
    to get replaced by a single token, \code{new york}.

    By default, we set \code{combine = abbreviations("english")}, so
    that abbreviations like \code{"Ms."} get treated as single tokens;
    with \code{combine = NULL}, trailing punctuation gets split off, and
    \code{"Ms."} gets tokenized into two tokens, for "Ms" and ".".
}
\value{
    \code{text_tokens} returns a list of the same length as \code{x}, with
    the same names. Each list item is a character vector with the tokens
    for the corresponding element of \code{x}.

    \code{text_ntoken} returns a numeric vector the same length as \code{x},
    with each element giving the number of non-dropped tokens in the
    corresponding text.
}
\seealso{
    \code{\link{text_split}}, \code{\link{text_types}},
    \code{\link{abbreviations}}, \code{\link{stopwords}},
    \code{\link{term_matrix}}.
}
\examples{
    text_tokens("The quick ('brown') fox can't jump 32.3 feet, right?")

    # count non-dropped tokens:
    text_ntoken("The quick ('brown') fox can't jump 32.3 feet, right?")

    # don't normalize:
    f <- text_filter(map_case = FALSE, map_compat = FALSE,
                     map_quote = FALSE, remove_ignorable = FALSE)
    text_tokens("The quick ('brown') fox can't jump 32.3 feet, right?", f)

    # drop common function words ('stop' words):
    text_tokens("Able was I ere I saw Elba.",
                text_filter(drop = stopwords("english")))

    # drop numbers, with some exceptions:"
    text_tokens("0, 1, 2, 3, 4, 5",
                text_filter(drop_number = TRUE,
                            drop_except = c("0", "2", "4")))

    # apply stemming...
    text_tokens("Mary is running", text_filter(stemmer = "english"))

    # ...except for certain words
    text_tokens("Mary is running",
                text_filter(stemmer = "english", stem_except = "mary"))

    # combine abbreviations by default
    text_tokens("Ms. Jones")

    # disable default combinations
    text_tokens("Ms. Jones", text_filter(combine = NULL))

    # add new combinations
    text_tokens("Ms. Jones is from New York City, New York.",
                text_filter(combine = c(abbreviations("english"),
                                        "new york", "new york city")))
}

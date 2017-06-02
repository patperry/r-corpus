\name{tokens}
\alias{corpus_token_filter}
\alias{token_filter}
\alias{tokens}
\title{Text Tokenization}
\description{
   Segment text into tokens, each of which is an instance of
   a particular \sQuote{type}.
}
\usage{
    token_filter(map_case = TRUE, map_compat = TRUE, map_quote = TRUE, 
                 remove_ignorable = TRUE, ignore_space = TRUE,
                 stemmer = NULL, stem_except = drop, combine = NULL,
                 drop_letter = FALSE, drop_mark = FALSE, drop_number = FALSE,
                 drop_punct = FALSE, drop_symbol = FALSE, drop_other = FALSE,
                 drop = NULL, drop_except = NULL)

    tokens(x, filter = token_filter())
}
\arguments{
    \item{x}{object to be tokenized.}

    \item{filter}{filter specifying the transformation from text to
        token sequence, a list or token filter object.}

    \item{map_case}{a logical value indicating whether to apply Unicode
        case mapping to the text. For most languages, this transformation
        changes uppercase characters to their lowercase equivalents.}

    \item{map_compat}{a logical value indicating whether to apply
        Unicode compatibility mappings to the characters, those required
        for NFKC and NFKD normal forms.}

    \item{map_quote}{a logical value indicating whether to replace Unicode
        quote characters like single quote, double quote, and apostrophe,
        with an ASCII single quote (').}

    \item{remove_ignorable}{a logical value indicating whether to remove
        Unicode "default ignorable" characters like zero-width spaces
        and soft hyphens.}

    \item{ignore_space}{a logical value indicating whether to ignore
        white-space-only tokens.}

    \item{stemmer}{a character value giving the name of the stemming
        algorithm, or \code{NULL} to leave words unchanged. The stemming
        algorithms are provided by the
        \href{http://snowballstem.org/algorithms/}{Snowball stemming library};
        the following stemming algorithms are available:
            \code{"arabic"}, \code{"danish"}, \code{"dutch"},
            \code{"english"}, \code{"finnish"}, \code{"french"},
	        \code{"german"}, \code{"hungarian"}, \code{"italian"},
            \code{"norwegian"}, \code{"porter"}, \code{"portuguese"},
	        \code{"romanian"}, \code{"russian"}, \code{"spanish"},
            \code{"swedish"}, \code{"tamil"}, and \code{"turkish"}.
        }

    \item{stem_except}{a character vector of exception words to exempt from
        stemming, or \code{NULL}. If left unspecified, \code{stem_except}
        is set equal to the \code{drop} argument.}

    \item{combine}{a character vector of multi-word phrases to combine, or
        \code{NULL}; see \sQuote{Combining words}.}

    \item{drop_letter}{a logical value indicating whether to replace
        \code{"letter"} tokens (cased letters, kana, idoegraphic, letter-like
        numeric characters and other letters) with \code{NA}.}

    \item{drop_mark}{a logical value indicating whether to replace
        \code{"mark"} tokens (subscripts, superscripts, modifier letters,
        modifier symbols, and other marks) with \code{NA}.}

    \item{drop_number}{a logical value indicating whether to replace
        \code{"number"} tokens (decimal digits, words appearing to be
        numbers, and other numeric characters) with \code{NA}.}

    \item{drop_punct}{a logical value indicating whether to replace
        \code{"punct"} tokens (punctuation) with \code{NA}.}

    \item{drop_symbol}{a logical value indicating whether to replace
        \code{"symbol"} tokens (emoji, math, currency, and other symbols)
        with \code{NA}.}

    \item{drop_other}{a logical value indicating whether to replace
        \code{"other"} tokens (types that do not fall into any other
        categories) with \code{NA}.}

    \item{drop}{a character vector of types to replace with \code{NA},
        or \code{NULL}.}

    \item{drop_except}{a character of types to exempt from the drop
        rules specified by the \code{drop_letter}, \code{drop_mark},
        \code{drop_number}, \code{drop_punct}, \code{drop_symbol},
        \code{drop_other}, and \code{drop} arguments, or \code{NULL}.}
}
\details{
    \code{tokens} splits texts into token sequences. Each token is an instance
    of a particular type. This operation proceeds in a series
    of stages, controlled by the \code{filter} argument:

    \enumerate{
        \item First, we segment the text into words using the boundaries
            defined by
            \href{http://unicode.org/reports/tr29/#Word_Boundaries}{Unicode
                Standard Annex #29, Section 4}. We categorize each word as
            \code{"space"}, \code{"letter"}, \code{"mark"}, \code{"number"},
            \code{"punct"}, \code{"symbol"}, or \code{"other"} according
            to the first character in the word.  For words with two or
            more characters that start with extenders like underscore
            (\code{_}), we use the second character in the word to
            categorize it, treating a second extender as a letter.

        \item If \code{ignore_space} is \code{TRUE}, the we delete all
            \code{"space"} words.

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
            \code{stem_except} character vector.

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
            or \code{drop_other} are \code{TRUE}, we replace the tokens
            with values in the corresponding categories by \code{NA}.
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
    \code{FALSE} and all other properties as \code{NULL}.
}
\section{Combining words}{
    The \code{combine} property of a \code{token_filter} enables
    transformations that combine two or more words into a single token. For
    example, specifying \code{combine = "new york"} will
    cause consecutive instances of the words \code{new} and \code{york}
    to get replaced by a single token, \code{new york}.
}
\value{
    A list of the same length as \code{x}, with the same names. Each list
    item is a character vector with the tokens for the corresponding
    element of \code{x}.
}
\seealso{
    \code{\link{text_split}}, \code{\link{stopwords}},
    \code{\link{term_counts}}, \code{\link{term_matrix}}.
}
\examples{
    tokens("The quick ('brown') fox can't jump 32.3 feet, right?")

    # don't normalize:
    tokens("The quick ('brown') fox can't jump 32.3 feet, right?", NULL)

    # drop common function words ('stop' words):
    tokens("Able was I ere I saw Elba.",
           token_filter(drop = stopwords("english")))

    # drop numbers, with some exceptions:"
    tokens("0, 1, 2, 3, 4, 5",
           token_filter(drop_number = TRUE, drop_except = c("0", "2", "4")))
}

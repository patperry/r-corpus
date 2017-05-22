\name{tokens}
\alias{text_filter}
\alias{tokens}
\title{Text Tokenization}
\description{
   Segment text into tokens, each of which is an instance of
   a particular \sQuote{term} (formally, a type).
}
\usage{
    text_filter(map_case = TRUE, map_compat = TRUE,
                map_dash = TRUE, map_quote = TRUE, 
                remove_control = TRUE, remove_ignorable = TRUE,
                remove_space = TRUE, ignore_empty = TRUE,
                stemmer = NULL, stem_except = drop, combine = NULL,
                drop_symbol = FALSE, drop_number = FALSE,
                drop_letter = FALSE, drop_kana = FALSE,
                drop_ideo = FALSE, drop = NULL, drop_except = select,
                select = NULL)

    tokens(x, filter = text_filter())
}
\arguments{
    \item{x}{object to be tokenized.}

    \item{filter}{filter specifying the transformation from text to
        token sequence, a list or \code{text_filter} object.}

    \item{map_case}{a logical value indicating whether to apply Unicode
        case mapping to the text. For most languages, this transformation
        changes uppercase characters to their lowercase equivalents.}

    \item{map_compat}{a logical value indicating whether to apply
        Unicode compatibility mappings to the characters, those required
        for NFKC and NFKD normal forms.}

    \item{map_dash}{a logical value indicating whether to replace Unicode
        dash characters like em dash and en dash with an ASCII dash (-).}

    \item{map_quote}{a logical value indicating whether to replace Unicode
        quote characters like single quote, double quote, and apostrophe,
        with an ASCII single quote (').}

    \item{remove_control}{a logical value indicating whether to remove
        non-white-space control characters (from the C0 and C1 character
        classes, and the delete characer).}

    \item{remove_ignorable}{a logical value indicating whether to remove
        Unicode "default ignorable" characters like zero-width spaces
        and soft hyphens.}

    \item{remove_space}{a logical value indicating whether to remove
        white-space characters like space and new line.}

    \item{ignore_empty}{a logical value indicating whether to ignore tokens
        which, after applying all other normalizations, are empty (containing
        no characters). A token can become empty if, for example, it starts
        as white-space.}

    \item{stemmer}{a character value giving the name of the stemming
        algorithm, or \code{NULL} to leave words unchanged. The stemming
        algorithms are provided by the
        \href{http://snowballstem.org/algorithms/}{Snowball stemming library};
        the following stemming algorithms are available:
            arabic, danish, dutch, english, finnish, french,
	        german, hungarian, italian, norwegian, porter, portuguese,
	        romanian, russian, spanish, swedish, tamil, and turkish.
        }

    \item{stem_except}{a character vector of exception words to exempt from
        stemming, or \code{NULL}. If left unspecified, \code{stem_except}
        is set equal to the \code{drop} argument.}

    \item{combine}{a character vector of multi-word phrases to combine, or
        \code{NULL}; see \sQuote{Combining words}.}

    \item{drop_symbol}{a logical value indicating whether to replace symbol
        terms (punctuation, emoji, and other words that are not classified
        as number, letter, kana, or ideo) with \code{NA}.}

    \item{drop_number}{a logical value indicating whether to replace terms
        that appear to be numbers with \code{NA}.}

    \item{drop_letter}{a logical value indicating whether to replace terms
        starting with letters---excluding hiragana, katakana, and ideographic
        characters---with \code{NA}.}

    \item{drop_kana}{a logical value indicating whether to replace terms
        starting kana characters with \code{NA}.}

    \item{drop_ideo}{a logical value indicating whether to replace terms
        starting with ideographic characters with \code{NA}.}

    \item{drop}{a character vector of terms to replace with \code{NA},
        or \code{NULL}.}

    \item{drop_except}{a character of terms to exempt from the drop
        rules specified by the \code{drop_symbol}, \code{drop_number},
        \code{drop_letter}, \code{drop_kana}, \code{drop_ideo}, and
        \code{drop} arguments, or \code{NULL}. If left unspecified,
        \code{drop_except} is set equal to the \code{select} argument.}

    \item{select}{a character vector of terms to keep, or
        \code{NULL}; if non-\code{NULL}, tokens that are not on
        this list get replaced with \code{NA}.}
}
\details{
    \code{tokens} splits texts into token sequences. Each token is an instance
    of a particular term (formally, type). This operation proceeds in a series
    of stages, controlled by the \code{filter} argument:

    \enumerate{
        \item First, we segment the text into words using the boundaries
            defined by
            \href{http://unicode.org/reports/tr29/#Word_Boundaries}{Unicode
                Standard Annex #29, Section 4}. We categorize each word as
            \code{number}, \code{letter}, \code{kana}, \code{ideo}, or
            \code{symbol} according to whether the first character is a
            number, letter, kana, ideographic, or other character,
            respectively. For words with two or more characters that start
            with extenders like underscore (\code{_}), we use the second
            character in the word to categorize it, treating a second
            extender as a letter.

        \item Next, we normalize the words by applying the
            character mappings indicated by the \code{map_case},
            \code{map_compat}, \code{map_dash}, \code{map_quote},
            \code{remove_control}, \code{remove_ignorable}, and
            \code{remove_space} properties. If, after normalization, a
            word is empty (for example, if it started out as all white-space
            and \code{remove_space} is \code{TRUE}), and if
            \code{ignore_empty} is \code{TRUE}, we delete the word from
            the sequence.  At the end of the second stage, we have segmented
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
            for that term; otherwise, we create a single-word token. See the
            \sQuote{Combining words} section below for more details.  After
            this stage, the sequence elements are \sQuote{tokens}, not
            \sQuote{words}.

        \item If any of \code{drop_symbol}, \code{drop_number},
            \code{drop_letter}, \code{drop_kana}, or \code{drop_ideo}
            are \code{TRUE}, we replace the terms in the
            corresponding categories by \code{NA}. (For multi-word terms,
            we take the category of the first word in the phrase.)
            Then, if the \code{drop} property is non-\code{NULL}, we replace
            terms that match elements of this character vector with
            \code{NA}. We can add exceptions to the drop rules by specifying
            a non-\code{NULL} value for the \code{drop_except} property:
            \code{drop_except} is a character vector, then we we restore
            terms that match elements of vector to their values prior to
            dropping.

        \item Finally, if \code{select} is non-\code{NULL}, we replace
            terms that do not match elements of this character vector
            with \code{NA}.
    }

    When \code{filter = NULL}, we treat all logical properties as
    \code{FALSE} and all other properties as \code{NULL}.
}
\section{Combining words}{
    The \code{combine} property of a \code{text_filter} enables
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
    \code{\link{sentences}}, \code{\link{term_counts}}.
}
\examples{
    tokens("The quick ('brown') fox can't jump 32.3 feet, right?")

    # don't normalize:
    tokens("The quick ('brown') fox can't jump 32.3 feet, right?", NULL)

    # drop common function words ('stop' words):
    tokens("Able was I ere I saw Elba.",
           text_filter(drop = stopwords("english")))

    # drop numbers, with some exceptions:"
    tokens("0, 1, 2, 3, 4, 5",
           text_filter(drop_number = TRUE, drop_except = c("0", "2", "4")))
}

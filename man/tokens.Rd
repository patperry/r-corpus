\name{tokens}
\alias{text_filter}
\alias{tokens}
\title{Text Tokenization}
\description{
   Segment text into tokens, each of which is an instance of
   a particular \sQuote{term} (word type).
}
\usage{
    text_filter(fold_case = TRUE, fold_dash = TRUE,
                fold_quote = TRUE, map_compatible = TRUE,
                remove_control = TRUE, remove_ignorable = TRUE,
                remove_whitespace = TRUE, ignore_empty = TRUE,
                drop_symbol = FALSE, drop_number = FALSE,
                drop_letter = FALSE, drop_kana = FALSE,
                drop_ideo = FALSE, drop_words = NULL,
                stemmer = NULL, combine = NULL, select = NULL)

    tokens(x, filter = text_filter())
}
\arguments{
    \item{x}{object to be tokenized.}

    \item{filter}{filter to apply to the token sequence, or \code{NULL}.}

    \item{fold_case}{a logical value indicating whether to apply Unicode
        case folding to the text. For most languages, this transformation
        changes uppercase characters to their lowercase equivalents.}

    \item{fold_dash}{a logical value indicating whether to replace Unicode
        dash characters like em dash and en dash with an ASCII dash (-).}

    \item{fold_quote}{a logical value indicating whether to replace Unicode
        quote characters like single quote, double quote, and apostrophe,
        with an ASCII single quote (').}

    \item{map_compatible}{a logical value indicating whether to apply
        Unicode compatibility mappings to the characters, those required
        for NFKC and NFKD normal forms.}

    \item{remove_control}{a logical value indicating whether to remove
        non-whitespace control characters (from the C0 and C1 character
        classes, and the delete characer).}

    \item{remove_ignorable}{a logical value indicating whether to remove
        Unicode "default ignorable" characters like zero-width spaces
        and soft hyphens.}

    \item{remove_whitespace}{a logical value indicating whether to remove
        white space characters like space and new line.}

    \item{ignore_empty}{a logical value indicating whether to ignore tokens
        which, after applying all other normalizations, are empty (containing
        no characters). A token can become empty if, for example, it starts
        as white space.}

    \item{drop_symbol}{a logical value indicating whether to replace symbol
        words (punctuation, emoji, and other words that are not classified
        as number, letter, kana, or ideo) with \code{NA}.}

    \item{drop_number}{a logical value indicating whether to replace words
        that appear to be numbers with \code{NA}.}

    \item{drop_letter}{a logical value indicating whether to replace words
        containing of letters---excluding hiragana, katakana, and ideographic
        characters---with \code{NA}.}

    \item{drop_kana}{a logical value indicating whether to replace words
        containing kana characters with \code{NA}.}

    \item{drop_ideo}{a logical value indicating whether to replace words
        containing ideographic characters with \code{NA}.}

    \item{drop_words}{a character vector of words to replace with \code{NA},
        or \code{NULL}; words get matched against this list after
        normalization but before stemming.}

    \item{stemmer}{a character value giving the name of the stemming
        algorithm, or \code{NULL} to leave words unchanged. The stemming
        algorithms are provided by the
        \href{http://snowballstem.org/algorithms/}{Snowball stemming library};
        the following stemming algorithms are available:
            arabic, danish, dutch, english, finnish, french,
	        german, hungarian, italian, norwegian, porter, portuguese,
	        romanian, russian, spanish, swedish, tamil, and turkish.
        }

    \item{combine}{a two-column character matrix of tokens to
        combine, or \code{NULL}; see \sQuote{Combining tokens}.}

    \item{select}{a character vector of tokens to keep, or
        \code{NULL}; if non-\code{NULL}, tokens that are not on
        this list get replaced with \code{NA}.}
}
\details{
    \code{tokens} splits texts into token sequences. This operation
    proceeds in a series of stages, controlled by the \code{filter}
    argument:

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
            character mappings indicated by the \code{fold_case},
            \code{fold_dash}, \code{fold_quote}, \code{map_compatible},
            \code{remove_control}, \code{remove_ignorable}, and
            \code{remove_whitespace} properties. If, after normalization, a
            word is empty (for example, if it started out as all white space
            and \code{remove_whitespace} is \code{TRUE}), and if
            \code{ignore_empty} is \code{TRUE}, we delete the word from
            the sequence.  At the end of the second stage, we have segmented
            the text into a sequence of normalized words, in Unicode composed
            normal form (NFC, or if \code{map_compatible} is \code{TRUE},
            NFKC).

        \item In the third stage, if any of \code{drop_symbol},
            \code{drop_number}, \code{drop_letter}, \code{drop_kana},
            or \code{drop_ideo} are \code{TRUE}, we replace the word tokens
            in the corresponding categories by \code{NA}. Also, if the
            \code{drop_words} property is non-\code{NULL}, we replace
            the word tokens that match elements of this character vector
            with \code{NA}.

        \item In the fourth stage, if the \code{stemmer} property is
            non-\code{NULL}, we apply the indicated stemming algorithm,
            replacing the non-\code{NA} word tokens with their stemmed forms.
            After this stage, we refer to the sequence elements as
            \sQuote{tokens}, not \sQuote{words}.

        \item Next, if the \code{combine} property is non-\code{NULL},
            we iteratively replace two-token sequences that match rows of the
            \code{combine} matrix with combined tokens. See the
            \sQuote{Combining tokens} section below for more details.

        \item Finally, if \code{select} is non-\code{NULL}, we replace
            tokens that do not match elements of this character vector with
            \code{NA}.
    }

    When \code{filter = NULL}, we treat all logical properties as
    \code{FALSE} and all other properties as \code{NULL}.
}
\section{Combining tokens}{
    The \code{combine} property of a \code{text_filter} enables
    transformations that combine two tokens into a single entity. For
    example, specifying \code{combine = cbind("new", "york")} will
    cause consecutive instances of the tokens \code{new} and \code{york}
    to get replaced by a single token, \code{new york}.

    When non-\code{NULL}, the \code{combine} property should be
    a two-column character matrix. Each row of this matrix corresponds
    to a combination rule, with the two columns corresponding to the left and
    right tokens in the combination, in that order.

    The elements of the \code{combine} argument are tokens, not
    words. This allows you to form multi-word tokens incrementally as
    two-token combinations. For example, appending the row
    \code{cbind("new york", "city")} to the \code{combine} property
    would cause each instance of the string \code{new york city} to
    be treated as a single token.
}
\value{
    A list of the same length as \code{x}, with the same names. Each list
    item is a character vector with the tokens for the corresponding
    element of \code{x}.
}
\seealso{
    \code{\link{sentences}}.
}
\examples{
    tokens("The quick ('brown') fox can't jump 32.3 feet, right?")

    # don't normalize:
    tokens("The quick ('brown') fox can't jump 32.3 feet, right?", NULL)

    # drop common function words ('stop' words)
    tokens("Able was I ere I saw Elba.",
           text_filter(drop_words = stopwords("english")))
}

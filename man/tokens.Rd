\name{tokens}
\alias{text_filter}
\alias{tokens}
\title{Text Tokenization}
\description{
   Segment text into tokens, each of which is an instance of
   a particular word type (normalized token).
}
\usage{
    text_filter(fold_case = TRUE, fold_dash = TRUE,
                fold_quote = TRUE, map_compatible = TRUE,
                remove_control = TRUE, remove_ignorable = TRUE,
                remove_whitespace = TRUE, ignore_empty = TRUE,
                drop_symbol = FALSE, drop_number = FALSE,
                drop_letter = FALSE, drop_kana = FALSE,
                drop_ideo = FALSE, stemmer = NULL)

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

    \item{stemmer}{a character value giving the name of the stemming
        algorithm, or \code{NULL} to disable stemming. The stemming
        algorithms are provided by the
        \href{http://snowballstem.org/algorithms/}{Snowball stemming library};
        the following stemming algorithms are available:
            arabic, danish, dutch, english, finnish, french,
	        german, hungarian, italian, norwegian, porter, portuguese,
	        romanian, russian, spanish, swedish, tamil, and turkish.
        }
}
\details{
    \code{tokens} splits text at the word boundaries defined by
    \href{http://unicode.org/reports/tr29/#Word_Boundaries}{Unicode Standard Annex #29, Section 4},
    normalizes the text to Unicode NFC normal form, and then applies
    a series of further transformations to the resulting tokens
    as specified by the \code{filter} argument. To skip the addtional
    transformation step, specify \code{filter = NULL}.
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
}

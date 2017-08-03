\name{corpus-deprecated}
\alias{corpus-deprecated}
\alias{token_filter}
\alias{sentence_filter}
\title{Deprecated Functions in Package \pkg{corpus}}
\description{
  These functions are provided for compatibility with older versions of
  \pkg{corpus} only, and may be defunct as soon as the next release.
}
\usage{
    token_filter(map_case = TRUE,
                 map_quote = TRUE, remove_ignorable = TRUE,
                 stemmer = NULL, stem_except = drop,
                 combine = abbreviations("english"),
                 drop_letter = FALSE, drop_mark = FALSE,
                 drop_number = FALSE, drop_punct = FALSE,
                 drop_symbol = FALSE, drop_other = FALSE,
                 drop = NULL, drop_except = NULL)

    sentence_filter(crlf_break = FALSE,
                    suppress = abbreviations("english"))
}
\arguments{
    \item{map_case}{a logical value indicating whether to apply Unicode
        case mapping to the text. For most languages, this transformation
        changes uppercase characters to their lowercase equivalents.}
    \item{map_quote}{a logical value indicating whether to replace curly
        single quotes and double quotes with straight single quotes
        and double quotes.}
    \item{remove_ignorable}{a logical value indicating whether to remove
        Unicode "default ignorable" characters like zero-width spaces
        and soft hyphens.}
    \item{stemmer}{a character value giving the name of the stemming
        algorithm, or \code{NA} to leave words unchanged. The stemming
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
    \item{crlf_break}{a logical value indicating whether to break
        sentences on carriage returns or line feeds.}
    \item{suppress}{a character vector of sentence break suppressions.}
}
\details{
    The \code{token_filter} and \code{sentence_filter} functions are
    deprecated; use \code{\link{text_filter}} instead.
}
\seealso{
  \code{\link{Deprecated}}, \code{\link{text_filter}}
}
\keyword{internal}
\keyword{misc}

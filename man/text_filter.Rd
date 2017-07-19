\name{text_filter}
\alias{corpus_text_filter}
\alias{text_filter}
\alias{text_filter.corpus_text}
\alias{text_filter.data.frame}
\alias{text_filter.default}
\alias{text_filter<-}
\alias{text_filter<-.corpus_text}
\alias{text_filter<-.data.frame}
\alias{text_filter<-.default}
\title{Text Filters}
\description{
   Get or specify the process by which text gets transformed into
   a sequence of tokens or sentences.
}
\usage{
    text_filter(x = NULL, ...)
    text_filter(x) <- value

    \method{text_filter}{corpus_text}(x = NULL, ...)

    \method{text_filter}{data.frame}(x = NULL, ...)

    \method{text_filter}{default}(x = NULL, ...,
                        map_case = TRUE, map_compat = TRUE,
                        map_quote = TRUE, remove_ignorable = TRUE,
                        stemmer = NULL, stem_dropped = FALSE,
                        stem_except = NULL,
                        combine = abbreviations("english"),
                        drop_letter = FALSE, drop_mark = FALSE,
                        drop_number = FALSE, drop_punct = FALSE,
                        drop_symbol = FALSE, drop_other = FALSE,
                        drop_url = FALSE,
                        drop = NULL, drop_except = NULL,
                        sent_crlf = FALSE,
                        sent_suppress = abbreviations("english"))
}
\arguments{
    \item{x}{text or corpus object.}

    \item{value}{text filter object, or \code{NULL} for the default.}

    \item{...}{further arguments passed to or from other methods.}

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

    \item{stem_dropped}{a logical value indicating whether to stem words
        in the \code{"drop"} list.}

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

    \item{drop_url}{a logical value indicating whether to replace
        \code{"url"} tokens (web addresses starting with \code{"http:/"} or
        \code{"https:/"}) with \code{NA}.}

    \item{drop}{a character vector of types to replace with \code{NA},
        or \code{NULL}.}

    \item{drop_except}{a character of types to exempt from the drop
        rules specified by the \code{drop_letter}, \code{drop_mark},
        \code{drop_number}, \code{drop_punct}, \code{drop_symbol},
        \code{drop_other}, \code{drop_url}, and \code{drop} arguments,
        or \code{NULL}.}

    \item{sent_crlf}{a logical value indicating whether to break
        sentences on carriage returns or line feeds.}

    \item{sent_suppress}{a character vector of sentence break suppressions.}
}
\details{
    The set of properties in a text filter determine the tokenization
    and sentence breaking rules. See the documentation for
    \code{\link{text_tokens}} and \code{\link{text_split}} for details
    on the tokenization process.
}
\value{
    \code{text_filter} retrieves an objects text filter, optionally
    with modifications to some of its properties.

    \code{text_filter<-} sets an object's text filter. Setting the
    text filter on a character object is not allowed; the object must
    have type \code{"corpus_text"} or be a data frame with a \code{"text"}
    column of type \code{"corpus_text"}.
}
\seealso{
    \code{\link{as_text}}, \code{\link{text_tokens}},
    \code{\link{text_split}}, \code{\link{abbreviations}},
    \code{\link{stopwords}}.
}
\examples{
    # text filter with default options set
    text_filter()

    # specify some options but leave others unchanged
    f <- text_filter(map_case = FALSE, drop = stopwords("english"))

    # set the text filter property
    x <- as_text(c("Marnie the Dog is #1 on the internet."))
    text_filter(x) <- f
    text_tokens(x) # by default, uses x's text_filter to tokenize

    # change a filter property
    f2 <- text_filter(x, map_case = TRUE)
    # equivalent to:
    # f2 <- text_filter(x)
    # f2$map_case <- TRUE

    text_tokens(x, f2) # override text_filter(x)

    # setting text_filter on a data frame is allowed if it has a
    # column names "text" of type "corpus_text"
    d <- data.frame(text = x)
    text_filter(d) <- f2
    text_tokens(d)

    # but you can't set text filters on character objects
    y <- "hello world"
    \dontrun{text_filter(y) <- f2} # gives an error

    d2 <- data.frame(text = "hello world", stringsAsFactors = FALSE)
    \dontrun{text_filter(d2) <- f2} # gives an error
}

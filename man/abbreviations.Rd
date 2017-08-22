\name{abbreviations}
\alias{abbreviations}
\title{Abbreviations}
\description{
    Get a list of common abbreviations.
}
\usage{
abbreviations(kind = "english")
}
\arguments{
    \item{kind}{the name (or names) of the abbreviation list(s),
        \code{NA}, or \code{NULL}. Allowed values are \code{"english"},
        \code{"french"}, \code{"german"}, \code{"italian"},
        \code{"portuguese"}, and \code{"russian"}; these values
        retrieve the language-specific sentence break suppression
        lists compiled by the
        \href{http://cldr.unicode.org/}{Unicode Common Locale Data
            Repository},
        with a few additions to the \code{"english"} list.}
}
\details{
    \code{abbreviations} returns a character vector of abbreviations. The main
    use of this function is to get a list of sentence break suppressions,
    terms ending in full stops (periods, ambiguous sentence terminators) that
    when followed by an upper-case letter, do not signal the end of a
    sentence.

    The built-in abbreviation lists returned by this function are reasonable
    defaults, but they may require further tailoring to suit your particular
    task.
}
\value{
    A sorted character vector of unique abbreviations of the specified
    kind (or kinds if the \code{kind} argument is a vector), or \code{NULL}
    if \code{kind = NULL} or \code{kind = NA}.
}
\seealso{
    \code{\link{text_filter}}.
}
\examples{
head(abbreviations("english"))
head(abbreviations("french"))
abbreviations(NULL)

# multiple kinds:
head(abbreviations(c("italian", "portuguese")))

# add words to the default list:
my_abbrev <- c(abbreviations("english"), "Mon.", "Tues.", "Weds.")
}

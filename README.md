<div><img alt="Corpus" src="man/figures/banner.png" /></div>

[![Build Status (Linux)][travis-badge]][travis]
[![Build Status (Windows)][appveyor-badge]][appveyor]
[![Coverage Status][codecov-badge]][codecov]
[![CRAN Status][cran-badge]][cran]
[![CRAN RStudio Mirror Downloads][cranlogs-badge]][cran]


Overview
--------

*Corpus* is an R text processing package with full support for international
text (Unicode). It includes functions for reading data from newline-delimited
JSON files, for normalizing and tokenizing text, for searching for term
occurrences, and for computing term occurrence frequencies (including
n-grams).

*Corpus* does not provide any language models, part-of-speech tagging,
topic models, or word vectors.


Installation
------------

*Corpus* is [available on CRAN][cran]. To install the latest released version,
run the following command in R:

    install.packages("corpus")

*Note:* corpus *uses a git submodule, so `install_git` and `install_github`
won't work.  See the section on [building from source][building] below if you
want to install the development version.*


Performance
-----------

*Corpus* was designed for performance, with the majority of the package
implemented as a [standalone  C library][corpus].  Two benchmarks illustrate
its performance:

  + [reading in newline-delmited JSON data][bench-ndjson];

  + [computing a term frequency matrix][bench-term-matrix].

In both of these benchmarks, *corpus* is at least twice as fast as the next
competitor.


Usage
-----

### Data input

*Corpus* does not have a special "corpus" object. It uses data frames. It
does, however, have a special object for storing text, `corpus_text`, for the
`"text"` column of a data frame.

Read text data into R or convert an existing corpus using one of the following
functions:

 + `read_ndjson()` reads data in newline-delimited JSON format, optionally
   memory-mapping to enable processing large corpora that do not fit into
   RAM;

 + `corpus()` creates a new corpus: a data frame with a column named
   `"text"` of type `corpus_text`;

 + `as_corpus()` converts a character vector, data frame, or corpus
   object from another package (*quanteda*, *readtext*, or *tm*) to
   a data frame with a column named `"text"` of type `corpus_text`;

 + `as_text()` extracts the texts from a corpus object.

All *corpus* functions expecting text accept a variety of formats, including
*quanteda* and *tm* corpus objects. These functions call `as_text()` on their
inputs, then process the resulting `corpus_text` object.


### Preprocessing

You specify all preprocessing and text normalization (case folding, stemming,
stop word removal, etc.) using a *text filter*:

 + `text_filter()` gets or sets a text filter specifying token
   preprocessing and sentence boundaries.

Every `corpus_text` object has a text filter. You can get or set this filter
using the `text_filter()` function, or you can override the filter or specific
properties using the `filter` or `...` arguments of functions expecting text.


### Tokenizing text

*Corpus* conceives of texts as sequences of tokens, each of which is an
instance of a particular type.  To tokenize text or to compute its types,
use the following functions: 

 + `text_tokens()` transforms raw texts into token sequences.

 + `text_ntoken()` counts the number of tokens in each text.

 + `text_types()` computes the unique types in a set of texts.

 + `text_ntype()` counts the number of unique types.

The default text filter case folds the text, removes Unicode default ignorable
characters like zero-width spaces, and converts to Unicode normalized composed
form ([NFC][nfc]), and combines English abbreviations like `"Ms."` into single
tokens (for other words, trailing punctuation gets split off). For token
boundaries, *corpus* uses the word boundaries defined by [Unicode Standard
Annex #29, Section 4][wordbreak], with special rules for handling hyphens,
`@mentions`, `#hashtags`, and URLs.


### Segmenting text

*Corpus* can break text into sentences or token blocks:

 + `text_split()` segments text into sentences or blocks of tokens;

 + `text_sub()` extracts subsequences of tokens;

 + `text_length()` gets the number of dropped and non-dropped tokens in
   a set of texts;

 + `text_nsentence()` counts the number of sentences in a set of texts.

For sentence boundaries, *corpus* uses a tailored version of the boundaries
defined in [Unicode Standard Annex #29, Section 5][sentbreak]. Specifically,
when finding sentence boundaries, by default *corpus* treats carriage return
and new line like spaces, and *corpus* suppresses sentence breaks after
English abbreviations. You can override this behavior by using a different
`corpus_text_filter` constructed using the `text_filter()` function
mentioned above.


### Searching for terms

*Corpus* can search text for particular "terms" each of which is a sequence of
one or more types:

 + `text_locate()` reports all instances of tokens matching the search terms,
   along with contexts before and after the term instances;

 + `text_count()` counts the number of matches in each of a set of texts;

 + `text_detect()` indicates whether each text contains at least one of
   the search terms;

 + `text_subset()` gets the subset of texts containing a term.

Notably, each of these functions accepts a `corpus_token_filter` argument.
If this filter specifies a particular stemming behavior, then you search with
the stemmed type, and the search results will show the raw (unstemmed) text
that matches the term after tokenization.


### Tabulating term frequencies

*Corpus* can tabulate type or n-gram occurrence frequencies:

 + `term_stats()` for tabulating term occurrence statistics, aggregating
   over a set of texts.

 + `term_matrix()` for computing a term frequency matrix or its transpose
   (a "document-by-term matrix" or "term-by-document" matrix).
 
All three functions allow weighting the texts.  The `term_matrix()` function
allows you to select a specific term set, and also allow you to aggregate
over a specified a grouping factor.


### UTF-8 Handling

*Corpus* has functions for translating, validating, normalizing, and printing
UTF-8 encoded character data. These functions are useful for working around
[several][windows-enc2utf8] [bugs][emoji-print] in R's handling of UTF-8 data.
The [Unicdode vignette][unicode-vignette] describes these functions in detail.


Building from source
--------------------

To install the latest development version of the package, run the following
sequence of commands in R:

    local({
        # download the sources
        tmp <- tempfile()
        system2("git", c("clone", "--recursive",
                         shQuote("https://github.com/patperry/r-corpus.git"),
                         shQuote(tmp)))

        # install the package
        devtools::install(tmp)

        # optional: run the tests
        devtools::test(tmp)

        # optional: remove the temporary files
        unlink(tmp, recursive = TRUE)
    })

Note that the package uses a git submodule, so you cannot use
`devtools::install_github` to install it.


To obtain the source code, clone the repository and the submodules:

    git clone --recursive https://github.com/patperry/r-corpus.git

The `--recursive` flag is to make sure that the corpus library also gets
cloned. If you forget the `--recursive` flag, you can manually clone
the submodule with the following commands:

    cd r-corpus
    git submodule update --init

There are no other dependencies.


Contributing
------------

The project maintainer welcomes contributions in the form of feature requests,
bug reports, comments, unit tests, vignettes, or other code.  If you'd like to
contribute, either

 + fork the repository and submit a pull request (note the nonstandard
   instructions for [building from source][building]);

 + [file an issue][issues];

 + or contact the maintainer via e-mail.

This project is released with a [Contributor Code of Conduct][conduct],
and if you choose to contribute, you must adhere to its terms.


Acknowledgments
---------------

The API and feature set for *corpus* draw inspiration from
[*quanteda*][quanteda], developed by Ken Benoit and collaborators;
[*stringr*][stringr], developed by Hadley Wickham;
[*tidytext*][tidytext], developed by Julia Silge and David Robinson.


License
-------

*Corpus* is released under the [Apache Licence, Version 2.0][apache].


[apache]: https://www.apache.org/licenses/LICENSE-2.0.html "Apache License, Version 2.0"
[appveyor]: https://ci.appveyor.com/project/patperry/r-corpus/branch/master "Continuous Integration (Windows)"
[appveyor-badge]: https://ci.appveyor.com/api/projects/status/github/patperry/r-corpus?branch=master&svg=true "Continuous Inegration (Windows)"
[bench-term-matrix]: https://github.com/patperry/bench-term-matrix#readme "Term Matrix Benchmark"
[bench-ndjson]: https://github.com/jeroen/ndjson-benchmark#readme "NDJSON Benchmark"
[building]: #building-from-source "Building from Source"
[casefold]: https://www.w3.org/International/wiki/Case_folding "Case Folding"
[cc]: https://en.wikipedia.org/wiki/C0_and_C1_control_codes "C0 and C1 Control Codes"
[codecov]: https://codecov.io/github/patperry/r-corpus?branch=master "Code Coverage"
[codecov-badge]: https://codecov.io/github/patperry/r-corpus/coverage.svg?branch=master "Code Coverage"
[conduct]: https://github.com/patperry/r-corpus/blob/master/CONDUCT.md "Contributor Code of Conduct"
[corpus]: https://github.com/patperry/corpus "Corpus C Library"
[cran]: https://cran.r-project.org/package=corpus "CRAN Page"
[cran-badge]: http://www.r-pkg.org/badges/version/corpus "CRAN Page"
[cranlogs-badge]: http://cranlogs.r-pkg.org/badges/corpus "CRAN Downloads"
[emoji-print]: https://twitter.com/ptrckprry/status/887732831161425920 "MacOS Emoji Printing"
[issues]: https://github.com/patperry/r-corpus/issues "Issues"
[ndjson]: http://ndjson.org/ "Newline-Delimited JSON"
[nfc]: http://unicode.org/reports/tr15/ "Unicode Normalization Forms"
[quanteda]: http://quanteda.io/ "Quanteda"
[sentbreak]: http://unicode.org/reports/tr29/#Sentence_Boundaries "Unicode Text Segmentation, Sentence Boundaries"
[stringr]: http://stringr.tidyverse.org/ "Stringr"
[tidytext]: http://juliasilge.github.io/tidytext/ "Tidytext"
[travis]: https://travis-ci.org/patperry/r-corpus "Continuous Integration (Linux)"
[travis-badge]: https://api.travis-ci.org/patperry/r-corpus.svg?branch=master "Continuous Integration (Linux)"
[unicode-vignette]: http://corpustext.com/articles/unicode.html "Unicode: Emoji, accents, and international text"
[windows-enc2utf8]: https://twitter.com/ptrckprry/status/901494853758054401 "Windows enc2utf8 Bug"
[wordbreak]: http://unicode.org/reports/tr29/#Word_Boundaries "Unicode Text Segmentation, Word Boundaries"

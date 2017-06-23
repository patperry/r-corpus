Corpus (R Package)
==================

[![Build Status][travis-badge]][travis]
[![Coverage Status][codecov-badge]][codecov]
[![CRAN_Status_Badge][cran-badge]][cran]
![CRAN RStudio mirror downloads][cranlogs-badge]

Text corpus analysis in R, with full support for Unicode.


Overview
--------

*Corpus* is an R text processing package that includes are functions for
reading data from newline-delimited JSON files, for normalizing and tokenizing
text, for searching for term occurrences, and for computing term occurrence
frequencies (including n-grams).

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
implemented as a [standalone  C library][corpus].  The following benchmarks
demonstrate its capabilities:

  + [reading in newline-delmited JSON data][bench-ndjson];

  + [computing a term frequency matrix][bench-term-matrix].

In both of these benchmarks, *corpus* is at least twice as fast as the next
competitor.


Usage
-----

### Data input

For reading data or for converting an R object to type `corpus_text`, use
one of the following functions:

 + `read_ndjson()` reads data in newline-delimited JSON format, optionally
   memory-mapping to enable processing large corpora that do not fit into
   RAM;

 + `as_text()` converts an R object to a text object of type `corpus_text`.

All *corpus* functions expecting text accept both character vectors and data
frames. When the input argument is a data frame, *corpus* looks for a column
of type `corpus_text` or, if none exists, a column named `"text"`.


### Tokenizing text

*Corpus* conceives of texts as sequences of tokens, each of which is an
instance of a particular type.  To tokenize text or to compute its types,
use the following functions: 

 + `token_filter()` constructs a `corpus_token_filter` object specifying
   the process by which a text gets transformed into a token sequence
   (normalization, stemming, stop word removal, etc.).

 + `text_tokens()` transforms raw text into token sequences.

 + `text_ntoken()` counts the number of tokens in each text.

 + `text_types()` computes the unique types in a set of texts.

 + `text_ntype()` counts the number of unique types.

All *corpus* functions that need to tokenize text accept an argument named
`"filter"`, expecting a `corpus_token_filter` value that allows you to specify
the process by which raw a text gets transformed into a token sequence. The
default token filter case folds the text, removes Unicode default ignorable
characters like zero-width spaces, applies character compatibility maps
and converts to Unicode normalized composed form ([NFKC][nfkc]), and combines
English abbreviations like `"Ms."` into single tokens (for other words,
trailing punctuation gets split off). For token boundaries, *corpus*
uses the word boundaries defined by
[Unicode Standard Annex #29, Section 4][wordbreak].


### Segmenting text

*Corpus* can break text into sentences or token blocks:

 + `text_split()` segments text into sentences or blocks of tokens.

 + `sentence_filter()` constructs a `corpus_sentence_filter` controlling
   the sentence break behavior.

 + `text_nsentence()` counts the number of sentences in a set of texts.

For sentence boundaries, *corpus* uses a tailored version of the boundaries
defined in [Unicode Standard Annex #29, Section 5][sentbreak]. Specifically,
when finding sentence boundaries, by default *corpus* treats carriage return
and new line like spaces, and *corpus* suppresses sentence breaks after
English abbreviations. You can override this behavior by using a different
`corpus_sentence_filter` constructed using the `sentence_filter()` function.


### Searching for terms

*Corpus* can search text for particular "terms" each of which is a sequence of
one or more types:

 + `text_locate()` reports all instances of tokens matching the search terms,
   along with context before and after the tokens.

 + `text_count()` counts the number of matches in each of a set of texts.

 + `text_detect()` indicates whether each text contains at least one of
   the search terms.

Notably, each of these functions accepts a `corpus_token_filter` argument.
If this filter specifies a particular stemming behavior, then you search with
the stemmed type, and the search results will show the raw (unstemmed) text
that matches the term after tokenization.


### Tabulating term frequencies

*Corpus* can tabulate type or n-gram occurrence frequencies:

 + `term_counts()` for tabulating term occurrence frequencies, aggregating
   over a set of texts.

 + `term_matrix()` for computing a term frequency matrix or its transpose
   (a "document-by-term matrix" or "term-by-document" matrix).
 
 + `term_frame()` for computing a data frame with one row for each non-zero
   entry of the term matrix, with columns `"text"`, `"term"`, and
   `"count"`.

All three functions allow weighting the texts.  Both `term_matrix()` and
`term_frame()` allow selecting a specific term set, and they allow you
to specifying a grouping factor to aggregate over.


Building from source
--------------------

To install the latest development version of the package, run the following
sequence of commands in R:

    local({
        dir <- tempfile()
        cmd <- paste("git clone --recursive",
                     shQuote("https://github.com/patperry/r-corpus.git"),
                     shQuote(dir))
        system(cmd)
        devtools::install(dir)

        # optional: run the tests
        # must be in C locale for consistent string sorting
        collate <- Sys.getlocale("LC_COLLATE")
        Sys.setlocale("LC_COLLATE", "C")
        devtools::test(dir)
        Sys.setlocale("LC_COLLATE", collate) # restore the original locale

        # optional: remove the temporary files
        unlink(dir, recursive = TRUE)
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
bug reports, comments, or code.  If you'd like to contribute, either

 + fork the source code and submit a pull request (pay special attention to
   the instructions for [building from source][building]);

 + [file an issue][issues];

 + or contact the maintainer via e-mail.

This project is released with a [Contributor Code of Conduct][conduct],
and if you choose to contribute, you must adhere to its terms.


License
-------

*Corpus* is released under the [Apache Licence, Version 2.0][apache].


[apache]: https://www.apache.org/licenses/LICENSE-2.0.html "Apache License, Version 2.0"
[bench-term-matrix]: https://github.com/patperry/bench-term-matrix#readme "Term Matrix Benchmark"
[bench-ndjson]: https://github.com/jeroen/ndjson-benchmark#readme "NDJSON Benchmark"
[building]: https://github.com/patperry/r-corpus#building-from-source "Building from Source"
[casefold]: https://www.w3.org/International/wiki/Case_folding "Case Folding"
[cc]: https://en.wikipedia.org/wiki/C0_and_C1_control_codes "C0 and C1 Control Codes"
[codecov]: https://codecov.io/github/patperry/r-corpus?branch=master "Code Coverage"
[codecov-badge]: https://codecov.io/github/patperry/r-corpus/coverage.svg?branch=master "Code Coverage Badge"
[conduct]: https://github.com/patperry/r-corpus/blob/master/CONDUCT.md "Contributor Code of Conduct"
[corpus]: https://github.com/patperry/corpus "Corpus C Library"
[cran]: https://cran.r-project.org/package=corpus "Corpus CRAN Page"
[cran-badge]: http://www.r-pkg.org/badges/version/corpus "Corpus CRAN Badge"
[cranlogs-badge]: http://cranlogs.r-pkg.org/badges/corpus "Corpus CRAN Downloads Badge"
[issues]: https://github.com/patperry/r-corpus/issues "Issues"
[ndjson]: http://ndjson.org/ "Newline-Delimited JSON"
[nfkc]: http://unicode.org/reports/tr15/ "Unicode Normalization Forms"
[sentbreak]: http://unicode.org/reports/tr29/#Sentence_Boundaries "Unicode Text Segmentation, Sentence Boundaries"
[travis]: https://travis-ci.org/patperry/r-corpus "Continuous Integration"
[travis-badge]: https://api.travis-ci.org/patperry/r-corpus.svg?branch=master "Continuous Integration Badge"
[wordbreak]: http://unicode.org/reports/tr29/#Word_Boundaries "Unicode Text Segmentation, Word Boundaries"

Corpus (R package)
==================

Text corpus analysis in R. Heavy lifting is done by the
[Corpus C library][corpus].


Installing
----------

This package uses a git submodule, so you cannot use
`devtools::install_github` to install it. Instead, use the following command:

    devtools::install_git("git://github.com/patperry/r-corpus.git", args="--recursive")



Performance
-----------

Here are some performance comparisons for some basic operations.

### Extracting text from a JSON Lines file

The following benchmark reads in a 286 MB data file, `yelp-review.json`,
and extracts the text field from each row.  The raw data comes from the
first round of the [Yelp Dataset Challence][yelp]; it is stored in
[JSON Lines format][jsonl].

    # Using the corpus library:

    system.time({
        data <- read_json("yelp-review.json")
        text <- data$text
    })

    ##   user  system elapsed
    ##  1.235   0.079   1.316

    pryr::mem_used()

    ## 24.5 MB


    # Using the jsonlite library:

    system.time({
        data <- jsonlite::stream_in(file("yelp-review.json"), verbose=FALSE)
        text <- data$text
    })

    ##   user  system elapsed
    ## 36.788   0.588  37.522

    pryr::mem_used()

    ## 335 MB

We are about 30 times faster than `jsonlite`, and we use less than 10%
of the RAM.  How are we reading the file in only 24.5 MB?  We memory-map
the file, letting the operating system move data from the file to RAM
whenever necessary. We store the addresses of the text strings, but we
do not load values into RAM until they are needed.


### Segmenting text into sentences

The next benchmark segments each text into sentences, using the boundaries
defined by [Unicode Standard Annex #29, Section 5][sentbreak].

    # Using the corpus library:

    system.time({
        sents <- sentences(text)
    })

    ##   user  system elapsed
    ##  2.877   0.065   2.947

    pryr::mem_used()
    ## 135 MB


    # Using the stringi library:
    system.time({
        sents <- stringi::stri_split_boundaries(text, type="sentence")
    })

    ##   user  system elapsed
    ##  8.010   0.147   8.191

    pryr::mem_used()

    ## 843 MB

We are about 2.7 times faster than `stringi`, and we use 16% of the RAM.



### Tokenizing and normalizing text

The next benchmark performs a series of operations to transform each text into
a sequence of normalized tokens, called types. First, we segment each text
into word tokens using the boundaries defined by
[Unicode Standard Annex #29, Section 4][wordbreak]. Next, we normalize the
text into [Unicode NFKD normal form][nfkd], and we [case fold][casefold]
the text (for most languages, replacing uppercase with lowercase). Then,
we remove non-white-space [control characters][cc], default ignorable
characters like zero-width spaces, and white space. We also "dash fold,"
replacing Unicode dashes with an ASCII dash (-), and we "quote fold,"
replacing Unicode quotes (single, double, apostrophe) with ASCII single
quote ('). If, after normalization, the token is empty (for example if
it started out as white space), then we discard it.

    ## TODO: Add Demo

With `stringi`, it is more efficient to transform to NFKC normal form instead
of NFKD, and it is more efficient to do the word segmentation after the
normalization. The `stringi` package handles punctuation and white space
differently.

    system.time({
        ntext <- stringi::stri_trans_nfkc_casefold(text)
        tokens <- stringi::stri_extract_all_words(ntext)
    })

    ##   user  system elapsed
    ## 14.759   0.507  15.445

    pryr::mem_used()

    ## 958 MB



### Word usage statistics

In the final benchmark, we compute the word type occurrence frequencies for
each text, and we store them in a sparse matrix (a "document-by-term" count
matrix).

    ## TODO: Add Demo


Building from source
--------------------

To build the library from source, clone the repo and the submodules:

    git clone --recursive git://github.com/patperry/r-corpus.git

The `--recursive` flag is to make sure that the corpus library also gets
cloned. If you forget the `--recursive` flag, you can manually cloan
the submodule with the following command:

    git submodule update --init

There are no dependencies, but to build and run the tests, you will need
to install the [Check Unit Testing][check] library.


Windows support
---------------

Nonexistent. There are some notes about this in the [TODO file][windows] for
the [Corpus C library][corpus] project.

[casefold]: https://www.w3.org/International/wiki/Case_folding
[cc]: https://en.wikipedia.org/wiki/C0_and_C1_control_codes
[check]:https://libcheck.github.io/check/
[corpus]: https://github.com/patperry/corpus
[jsonl]: http://jsonlines.org/
[nfkd]: http://unicode.org/reports/tr15/
[sentbreak]: http://unicode.org/reports/tr29/#Sentence_Boundaries
[windows]: https://github.com/patperry/corpus/blob/master/TODO.md
[wordbreak]: http://unicode.org/reports/tr29/#Word_Boundaries
[yelp]: https://www.yelp.com/dataset_challenge

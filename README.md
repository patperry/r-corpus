<div style="margin-bottom:14px"><img alt="Corpus" src="man/figures/banner.png" /></div>

[![Build Status (Linux)][travis-badge]][travis]
[![Build Status (Windows)][appveyor-badge]][appveyor]
[![Coverage Status][codecov-badge]][codecov]
[![CRAN Status][cran-badge]][cran]
[![License][apache-badge]][apache]
[![CRAN RStudio Mirror Downloads][cranlogs-badge]][cran]


*Corpus* is an R text processing package with full support for international
text (Unicode). It includes functions for reading data from newline-delimited
JSON files, for normalizing and tokenizing text, for searching for term
occurrences, and for computing term occurrence frequencies (including
n-grams).

*Corpus* does not provide any language models, part-of-speech tagging, topic
models, or word vectors, but it can be used in conjunction with other packages
that provide these features.


Installation
------------

### Stable version

*Corpus* is [available on CRAN][cran].To install the latest released version,
run the following command in R:

    install.packages("corpus")


### Development version

To install the latest development version, run the following:

    tmp <- tempfile()
    system2("git", c("clone", "--recursive",
                     shQuote("https://github.com/patperry/r-corpus.git"), shQuote(tmp)))
    devtools::install(tmp)

Note that *corpus* uses a git submodule, so you cannot use
`devtools::install_github`.


Usage
-----

Here's how to get the most common non-punctuation, non-stop-word terms in *The
Federalist Papers*:

    > term_stats(federalist, drop = stopwords_en, drop_punct = TRUE)
       term         count support
    1  government     825      85
    2  state          787      85
    3  people         612      85
    4  one            544      85
    5  new            324      85
    6  york           151      85
    7  publius         85      85
    8  may            812      84
    9  states         845      82
    10 power          606      82
    11 must           446      81
    12 can            464      78
    13 every          350      77
    14 part           226      77
    15 constitution   462      76
    16 might          322      76
    17 general        255      76
    18 time           249      76
    19 great          291      74
    20 public         282      74
    ⋮  (8631 rows total)

Here's how to find all instances of tokens that stem to "power":

    > text_locate(federalist, "power", stemmer = "english")
       text             before              instance              after             
    1  1    …ay hazard a diminution of the   power   , emolument,\nand consequence …
    2  1    …s. So numerous indeed and so\n powerful  are the causes which serve to…
    3  1    … of a temper fond of despotic   power    and\nhostile to the principle…
    4  2    …der to vest it with requisite   powers  . It is well worthy\nof consid…
    5  2    …head of each the same kind of   powers   which they are advised to\npl…
    6  2    …\nwithout having been awed by   power   , or influenced by any passion…
    7  3    …ment, vested with sufficient\n  powers   for all general and national …
    8  3    … of nations towards all these   powers  , and to me it\nappears eviden…
    9  3    …he wrong themselves, nor want   power    or\ninclination to prevent or…
    10 3    …it will also be more in their   power    to\naccommodate and settle th…
    11 3    …cy of little consideration or   power   .\n\nIn the year 1685, the sta…
    12 3    …ain, or Britain, or any other  POWERFUL  nation?\n\nPUBLIUS.\n         
    13 4    … our advancement in union, in   power    and\nconsequence by land and …
    14 4    …t can apply the resources and   power    of the whole to the\ndefense …
    15 4    …\ncombining and directing the   powers   and resources of the whole, w…
    16 5    …h tend to beget and\nincrease   power    in one part and to impede its…
    17 6    … description are the love of\n  power    or the desire of pre-eminence…
    18 6    …nd dominion--the jealousy of\n  power   , or the desire of equality an…
    19 6    …rest of this enterprising and  powerful  monarch, he\nprecipitated Eng…
    20 6    …rprising a passion as that of   power    or glory? Have there not\nbee…
    ⋮  (912 rows total)

Here's how to get a term frequency matrix of all 1-, 2-, 3-, 4-, and 5-grams.

    > system.time(x <- term_matrix(federalist, ngrams = 1:5))
       user  system elapsed 
      7.592   0.140   7.795 

This computation uses only a single CPU, yet it still completes in under 10
seconds.


For a more complete introduction to the package, see the
[getting started guide][corpus-intro] and the other articles at
[corpustext.com](http://corpustext.com).


Citation
--------

Cite *corpus* with the following BibTeX entry:

    @Manual{,
        title = {corpus: Text Corpus Analysis},
        author = {Patrick O. Perry},
        year = {2017},
        note = {R package version 0.9.2},
        url = {http://corpustext.com},
    }


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
[*stringr*][stringr], developed by Hadley Wickham; [*tidytext*][tidytext],
developed by Julia Silge and David Robinson.


[apache]: https://www.apache.org/licenses/LICENSE-2.0.html "Apache License, Version 2.0"
[apache-badge]: https://img.shields.io/badge/License-Apache%202.0-blue.svg "Apache License, Version 2.0"
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
[corpus-intro]: http://corpustext.com/articles/corpus.html "Introduction to corpus"
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

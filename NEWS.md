corpus 0.9.4.9000
=================

### NEW FEATURES

  * Implement `length<-` for `corpus_text` objects.

  * Added `str()` method for `corpus_text` objects. Currently just a minimal
    implementation; this may change in the future.

### DEPRECATED AND DEFUNCT

  * Remove `text_length()`. Use `text_ntoken()` instead.

  * Remove `as_utf8()`, `utf8_valid()`, `utf8_normalize()`,
    `utf8_encode()`, `utf8_format()`, `utf8_print()`, and `utf8_width()`;
    these functions are in the *utf8* package now.


corpus 0.9.4 (2017-10-31)
=========================

### BUG FIXES

  * Fix bug in `print.corpus_frame(,row.names = FALSE)`.

  * Fix failing test on R-devel.

  * Fix failing tests on testthat 2.0.0.

### DEPRECATED AND DEFUNCT

  * Remove `weights` argument from `term_stats()` and `term_matrix()`.


corpus 0.9.3 (2017-10-02)
=========================

### NEW FEATURES

  * Allow user-supplied stemming functions in `text_filter()`.

  * Add `new_stemmer()` function to make a stemming function from a set
    of (term, stem) pairs.

  * Add `stem_snowball()` function for the Snowball stemming algorithms
    (similar to SnowballC::wordStem, but only stemming "letter" tokens,
    not "number", "punct", or "symbol").

  * Apply filter combine rules before stemming rather than after.

  * Remove dropped tokens rather than replace them with `NA`.

### MINOR IMPROVEMENTS

  * Replace white-space in types with connector (`_`).

  * Switch to `"radix"` sort algorithm for consistent, fast term ordering
    on all platforms, regardless of locale.

  * Set `combine = NULL` be default for text filters.

  * Make `map_quote` only change apostrophe and single quote characters,
    not double quote.

### BUG FIXES

  * Fix spurious rchk warnings.

  * Fix failing tests on R version 3.3.

### DEPRECATED AND DEFUNCT

  * Deprecate `text_length()` function in favor of `text_ntoken()`.

  * Removed deprecated functions `abbreviations()`, `as_corpus()`,
    `as_text()` `corpus()`, `is_corpus()`, `is_text()`, `stopwords()`,
    `term_frame()`.

  * Removed deprecated `random` argument from `text_locate()`.


corpus 0.9.2 (2017-09-20)
=========================

### NEW FEATURES

  * New package website, http://corpustext.com

  * Add support for tm `Corpus` and quanteda `corpus` objects; all functions
    expecting text (`text_tokens()`, `term_matrix()`, etc.) should work
    seamlessly on these objects.

  * Add `gutenberg_corpus()` for downloading a corpus from Project Gutenberg.

  * Add `...` arguments to all text functions, for overriding individual
    `text_filter()` properties.

  * Add `sentiment_afinn`, the AFINN sentiment lexicon.

  * Add `text_sample()` for getting a random sample of term instances.

  * Add `na.omit()`, `na.exclude()`, `na.fail()` implementations for
    `corpus_frame` and `corpus_text`.

### MINOR IMPROVEMENTS

  * Switch `as_utf8()` default argument to `normalize = FALSE`.

  * Re-order `as_corpus_text()` and `as_corpus_frame()` arguments; make
    both accept `...` arguments to override individual text filter properities.

  * Add missing single-letter initials to English abbreviation list.

  * Adaptively increase buffer size for `read_ndjson()` so that large
    files can be read quickly.

  * Make `summary()` on a `corpus_text` object report statistics for the
    number of tokens and types.

  * Switch to 2-letter language codes for stemming algorithms.

### BUG FIXES

  * Fix bug in `utf8_normalize()` when the input contains a backslash (`\`).

  * Fix bug in `term_matrix()` column names (non-ASCII names were getting
    replaced by Unicode escapes).

  * Work around R Windows bug in converting native to UTF-8; described at
    https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17329 .

  * Make comparison operations on text vectors keep names if arguments
    have them.

### DEPRECATED AND DEFUNCT

  * Renamed `corpus()`, `as_corpus()` and `is_corpus()` to `corpus_frame()`,
    `as_corpus_frame()` and `is_corpus_frame()` to avoid name clashes with
    other packages.

  * Renamed `as_text()` and `is_text()` to `as_corpus_text()` and
    `is_corpus_text()` to avoid name clashes with other packages.

  * Rename `term_frame()` to `term_counts()`.

  * Deprecate `text_locate()` `random` argument; use `text_sample()` instead.

  * Remove old deprecated `term_counts()` function; use `term_stats()` instead.

  * Deprecate `abbreviations()` and `stopwords()` function in favor of data
    objects: `abbreviations_en`, `stopwords_en`, `stopwords_fr`, etc.


corpus 0.9.1 (2017-08-20)
=========================

### BUG FIXES

  * Fix buffer overrun for case folding some Greek letters.

  * Fix memory leak in `read_ndjson()`.

  * Fix memory leak in JSON object deserialization.

  * Fix memory leak in `term_stats()`.

### DEPRECATED AND DEFUNCT

  * Rename `wnaffect` to `affect_wordnet`.


corpus 0.9.0 (2017-08-19)
=========================

### NEW FEATURES

  * Add `corpus()`, `as_corpus()`, `is_corpus()` functions.

  * Make `text_split()` split into evenly-sized blocks, with the 'size'
    argument specifying the maximum block size.

  * Added `text_stats()` function.

  * Added `text_match()` function to return matching terms as a factor.

  * Implemented text subset assignment operators `[<-` and `[[<-`.

  * Added `utf8_normalize()` function for translating to NFC normal form,
    applying case and compatibility maps.

  * Added `text_sub()` for getting token sub-sequences.

  * Added `text_length()` for text length, including `NA` tokens.

### MINOR IMPROVEMENTS

  * Add new vignette, "Introduction to corpus".

  * Add `random` argument to `text_locate` for random order.

  * Change `format.corpus_frame` to use elastic column widths for text.

  * Allow `rows = -1` for `print.corpus_frame` to print all rows.

  * Following quanteda, add "will" to the English stop word list.

  * Add special handling for hyphens so that, for example, "world-wide"
    is a single token (but "-world-wide-" is three tokens).

  * Merged "url" and "symbol" word categories.  Removed "other" word
    category (ignore these characters).

  * Change stemmer so that it only modifies tokens of kind "letter",
    preserving "number", "symbol", "url", etc.

  * Switched to more efficient `c.corpus_text()` function.

  * Make `text_locate()` return "text" column as a factor.

  * Constrain text `names()` to be unique, non-missing.

  * Added "names" argument to `as_text()` for overriding default names.

### BUG FIXES

  * Added checks for underflow in `read_ndjson()` double deserialization.

  * Fixed bug in `text_filter<-` where assignment did not make a deep copy
    of the object.

  * Fixed bug in `utf8_format()`, `utf8_print()`, `utf8_width()` where internal
    double quotes were not escaped.

  * Fixed rchk, UBSAN warnings.

### DEPRECATED AND DEFUNCT

  * Renamed `term_counts()` to `term_stats()`.

  * Removed deprecated functions `token_filter()` and `sentence_filter()`.

  * Removed `term` column from `text_locate()` output.

  * Removed `map_compat` option from `ext_filter()`; use `utf8_normalize()`
    instead if you need to apply compatibility maps.


corpus 0.8.0 (2017-07-18)
=========================

### NEW FEATURES

  * Added `text_filter()` generic.

  * Added `text_filter()<-` setter for text vectors.

  * Use a text's `text_filter()` attribute as a default in all `text_*`
    functions expecting a filter argument.

  * Added the _Federalist Papers_ dataset (`federalist`).

  * Added functions for validating and converting to UTF-8: `as_utf8()`,
    `utf8_valid()`.

  * Added functions for formatting and printing utf8 text: `utf8_encode()`,
    `utf8_format()`, `utf8_print()`, `utf8_valid()`, and `utf8_width()`.

### MINOR IMPROVEMENTS

  * Handle @mentions, #hashtags, and URLs in word tokenizer.

  * `term_counts()` now reports the `support` for each term (the number of
    texts containing the term), and has options for restricting output by
    the minimum and maximum support.

  * Added new class `corpus_frame` to support better printing of
    data frame objects: left-align text data, truncate output to screen
    width, display emoji on Mac OS. Use this class for all data frame
    return values.

  * Added a "unicode" vignette.

  * Converted the "chinese" demo to a vignette. Thanks to Will Lowe for
    contributing.

  * Make `text_split()` and `term_frame()` return parent text as a factor.

  * Remove `stringsAsFactors` option from `read_ndjson()`; deserialize all
    JSON string fields as character by default.

  * `read_ndjson()` de-serializes zero-length arrays as `integer()`,
    `logical()`, etc. instead of as `NULL`.

  * Allow user interrupts (control-C) in all long-running C computations.

### DEPRECATED AND DEFUNCT

  * Deprecate `token_filter()` and `sentence_filter()`.

### BUG FIXES

  * Fixed a bug where `read_ndjson()` would de-serialize a boolean `null`
    as `FALSE` instead of `NA`.


corpus 0.7.0 (2017-06-22)
=========================

### NEW FEATURES

  * Add `text_locate()`, for searching for specific terms in text,
    reporting the contexts of the occurrences ("Key words in context").

  * Add `text_count()` and `text_detect()` for counting term occurrences or
    checking for a term's existence in a text.

  * Add `text_types()` and `text_ntype()` for returning the unique types in
    a text, or counting types.

  * Add `text_nsentence()` for counting sentences.

  * Add `term_frame()`, reporting term frequencies as a data frame with
    columns `"text"`, `"term"`, and `"count"`.

### MINOR IMPROVEMENTS

  * Add transpose argument to `term_matrix()`.

  * Add new version of `format.corpus_text()` that is faster and aware
    of character widths, in particular, Emoji and East Asian character
    widths.

  * Normalize token filter `combine`, `drop`, `drop_except`, `stem_except`
    arguments, to allow passing cased versions of these arguments.

  * Set `combine = abbreviations("english")` by default.

### DEPRECATED AND DEFUNCT

  * Rename `tokens()` to `text_tokens()` for consistency; add `text_ntoken()`.

  * Rename `term_counts()` `min` and `max` arguments to `min_count` and
    `max_count`.

### BUG FIXES

  * Fixed bug where `"u.s"` (a unigram) stems to `"u.s"` (a bigram), and then
    causes for `term_matrix()` select.  Thanks to Dmitriy Selivanov for
    reporting: https://github.com/patperry/r-corpus/issues/3 .


corpus 0.6.0 (2017-06-06)
=========================

### NEW FEATURES

  * Add `ngrams` options for `term_counts()` and `term_matrix()`.

  * Add sentence break suppressions (special handling for abbreviations);
    the default behavior for `text_split(, "sentences")` is to use a set of
    English abbreviations as suppressions.

  * Add option to treat CR and LF like spaces when determining sentence
    boundaries; this is now the default.

### MINOR IMPROVEMENTS

  * Add `term_counts()` `min` and `max` options for excluding terms with
    counts below or above specified limits.

  * Add `term_counts()` `limit` option to limit the number of reported terms.

  * Add `term_counts()` `types` option for reporting the types that make up
    a term.

  * `Add abbreviations()` function with abbreviation lists for English,
    French, German, Italian, Portuguese, and Russian (from the Unicode
    Common Locale Data Repository).

  * Add more refined control over `token_filter()` drop cateogries:
    merged `"kana"`, and `"ideo"` into `"letter"`; split off `"punct"`,
    `"mark"`, and `"other"` from `"symbol"`.

### DEPRECATED AND DEFUNCT

  * Rename `text_filter()` to `token_filter()`.

  * Remove `select` argument from `token_filter()`, but add `select` to
    `term_matrix()` arguments.

  * Replace `sentences()` function with `text_split()`, which has options for
    breaking into multi-sentence blocks or multi-token blocks.

  * Remove `remove_control`, `map_dash`, and `remove_space` type
    normalization options from `text_filter()`.

  * Remove `ignore_empty` token filter option.


corpus 0.5.1 (2017-05-25)
=========================

### DEPRECATED AND DEFUNCT

  * Rename `"text"` class to `"corpus_text"` to avoid name classes with grid.
    Thanks to Jeroen Ooms for reporting:
    https://github.com/patperry/corpus/issues/1

  * Rename `"jsondata"` to `"corpus_json"` for consistency.

### BUG FIXES

  * Fix bug in `read_ndjson()` for reading factors with missing values.


corpus 0.5.0 (2017-05-23)
=========================

### NEW FEATURES

  * Add `term_counts()` function to tabulate term frequencies.

  * Add `term_matrix()` function to compute a term frequency matrix.

  * Add `text_filter()` option (`stem_except`) to exempt specific terms from
    stemming.

  * Add `text_filter()` option (`drop`) to drop specific terms, along with
    option (`drop_except`) to exempt specific terms from dropping.

  * Add `text_filter()` option (`combine`) to combine multi-word phrases like
    "new york city" into a single term.

  * `Add text_filter()` option (`select`) to select specific terms (excluding
    all words that are not on this list).

### MINOR IMPROVEMENTS

  * Add `stopwords()` function.

  * Make `read_ndjson()` decode JSON strings as character or factor (according
    to whether `stringsAsFactors` is `TRUE`) except for fields named `"text"`,
    which get decoded as text objects.

### DEPRECATED AND DEFUNCT

  * Rename `text_filter()` options `fold_case`, `fold_dash`, `fold_quote`
    to `map_case`, `map_dash`, `map_quote`.


corpus 0.4.0 (2017-05-16)
=========================

### NEW FEATURES

  * Allow `read_ndjson()` to read from connections, not just files, by
    reading the file contents into memory first. Use this by default
    instead of memory mapping.

### MINOR IMPROVEMENTS

  * Add `text_filter()` options `drop_symbol`, `drop_number`, `drop_letter`,
    `drop_kana`, and `drop_ideo`; these options replace the matched tokens
    with `NA`.

### BUG FIXES

  * Fix internal function namespace clashes on Linux and other similar
    platforms.

### DEPRECATED AND DEFUNCT

  * Rename `text_filter()` option `drop_empty` to `ignore_empty`.


corpus 0.3.0 (2017-05-04)
=========================

### NEW FEATURES

  * Support for serializing dataset and text objects via `readRDS()` and
    other native routines.  Unfortunately, this support doesn't come for
    free, and the objects take a little bit more memory.

  * Add support for stemming via the Snowball library.

### MINOR IMPROVEMENTS

  * More convenient interface for accessing JSON arrays.

  * Make `read_ndjson()` return a data frame by default, not a `"jsondata"`
    object.

### DEPRECATED AND DEFUNCT

  * Rename `as.text()`/`is.text()` to `as_text()`/`is_text()`; make
    `as_text()` retain names, work on S3 objects.

  * Rename `read_json()` to `read_ndjson()` to not clash with jsonlite.

  * Rename `"dataset"` type to `"jsondata"`.


corpus 0.2.0 (2017-04-15)
=========================

### NEW FEATURES

  * First CRAN release.

  * Added Windows support.

  * Added support for setting names on text objects.

  * Added documentation.


corpus 0.1.0 (2017-04-11)
=========================

### NEW FEATURES

  * First milestone, with support for JSON decoding, text segmentation,
    and text normalization.

To Do
=====

Bugs
----

 * `as_text` sets names for data frames that don't have them.


Features
--------

 * `ngram_counts()` returns data frame; `term_counts()` returns vector

 * `term_matrix()` for document-by-term (not `text_matrix()`); accepts
   `groups` and `weights` arguments for collapsing rows.

 * The concat operator `c()` does not work on text or jsondata. Should it?

 * Need implementations for the `.S3PrimitiveGenerics` for `jsondata` and
   `text`.

 * Need implementations for
   `names(.knownS3Generics)[.knownS3Generics == "base"]`
   for `jsondata` and `text`.

 * `summary.text`

 * `vocab()` for type/token frequencies

 * `textblocks()` for fixed-length chunks of words. Read up on time series block
   bootstrap to get inspiration for interface.

 * Bootstrapping. Not sure what the interface should look like, but should
   support sentence-level, word-level, block-level.

 * `read_ndjson` should make memory-mapping opt-in instead of the default,
   and should allow input from a connection when opting out.

To Do
=====


Features
--------

 * `ngram_counts()` returns data frame; `term_counts()` returns vector

 * `term_matrix()` for document-by-term (not `text_matrix()`); accepts
   `groups` and `weights` arguments for collapsing rows.

 * The concat operator `c()` does not work on text or dataset. Should it?

 * Need implementations for the `.S3PrimitiveGenerics` for `dataset` and
   `text`.

 * Need implementations for
   `names(.knownS3Generics)[.knownS3Generics == "base"]`
   for `dataset` and `text`.

 * `summary.text`

 * `vocab()` for type/token frequencies

 * `textblocks()` for fixed-length chunks of words. Read up on time series block
   bootstrap to get inspiration for interface.

 * Bootstrapping. Not sure what the interface should look like, but should
   support sentence-level, word-level, block-level.

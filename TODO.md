To Do
=====

Features
--------

 * `ngram_counts()`, analogous to `term_counts()`

 * `ngram_matrix()`, analogous to `term_matrix()`

 * Concat operator `c()` for text.

 * Need implementations for the `.S3PrimitiveGenerics` for `text`.

 * Need implementations for
   `names(.knownS3Generics)[.knownS3Generics == "base"]` for `text`.

 * `summary.text`

 * `vocab()` for type/token frequencies

 * `textblocks()` for fixed-length chunks of words. Read up on time series block
   bootstrap to get inspiration for interface.

 * Bootstrapping. Not sure what the interface should look like, but should
   support sentence-level, word-level, block-level.

To Do
=====


Features
--------

 * The concat operator `c()` does not work on text or dataset. Should it?

 * Need implementations for the `.S3PrimitiveGenerics` for `dataset` and
   `text`.

 * Need implementations for
   `names(.knownS3Generics)[.knownS3Generics == "base"]`
   for `dataset` and `text`.

 * `summary.text`

 * `vocab()` for type/token frequencies

 * `blocks()` for fixed-length chunks of words. Read up on time series block
   bootstrap to get inspiration for interface.

 * `word_counts()` (or `type_counts()`) for word count matrix; groups
   argument for collapsing rows.

 * Bootstrapping. Not sure what the interface should look like, but should
   support sentence-level, word-level, block-level.

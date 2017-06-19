To Do
=====

 * R strings are bounded by `INT_MAX`; check this.

Features
--------

 * `term_frame()`

 * Concat operator `c()` for text.

 * Need implementations for the `.S3PrimitiveGenerics` for `text`.

 * Need implementations for
   `names(.knownS3Generics)[.knownS3Generics == "base"]` for `text`.

 * `summary.text`

 * `vocab()` for type/token frequencies (?)

 * Some sort of key words in context (KWIC) function: `text_locate()`

 * See if text contains a term: `text_detect(,terms)`;
   also `text_subset(,terms)`

 * Need calls to `R_CheckUserInterrupt()` in long-running computations
   (without leaking memory).

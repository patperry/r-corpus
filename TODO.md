To Do
=====

 * R strings are bounded by `INT_MAX`; check this.

Features
--------

 * Concat operator `c()` for text.

 * Need implementations for the `.S3PrimitiveGenerics` for `text`.

 * Need implementations for
   `names(.knownS3Generics)[.knownS3Generics == "base"]` for `text`.

 * `summary.text`

 * `vocab()` for type/token frequencies (?)

 * `text_subset(,terms)`

 * Need calls to `R_CheckUserInterrupt()` in long-running computations
   (without leaking memory).

 * Passing a `token_filter` everywhere is annoying, error-prone. Allow
   setting a `filter` property on text objects to act as a default.

Todo
====

`read_json()` for reading JSON Lines, returning a `dataset` object.

`dataset.$` for extracting record fields. Arrays de-serialized as lists.

`dataset.[` for subsetting.

`text` data type.

`as.character.text()` for type conversion.

`tokens()` for word tokens.

`sentences()` for sentences.

`blocks()` for fixed-length chunks of words. Read up on time series block
bootstrap to get inspiration for interface.

`word_counts()` for word count matrix; groups argument for collapsing rows.

Bootstrapping. Not sure what the interface should look like, but should
support sentence-level, word-level, block-level.

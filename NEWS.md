# corpus 0.5.0

* Add `stopwords` function.


# corpus 0.4.0

* Allow `read_ndjson` to read from connections, not just files, by
  reading the file contents into memory first. Use this by default
  instead of memory mapping.

* Rename `drop_empty` to `ignore_empty` in `text_filter()`.

* Add `drop_symbol`, `drop_number`, etc. in `text_filter()`; these
  options replace the matched tokens with `NA`.

* Fix internal function namespace clashes on Linux and other similar
  platforms.


# corpus 0.3.0

* Support for serializing dataset and text objects (readRDS/saveRDS).
  Unfortunately, this support doesn't come for free, and the objects
  take a little bit more memory.

* More convenient interface for accessing JSON arrays.

* Rename `as.text`/`is.text` to `as_text`/`is_text`; make `as_text`
  retain names, work on S3 objects.

* Add support for stemming via the Snowball library.

* Rename `read_json` to `read_ndjson` to not clash with `jsonlite`.

* Rename `dataset` type to `jsondata`.

* Make `read_ndjson` return a data frame by default, not a `jsondata`
  object.


# corpus 0.2.0

* First CRAN release.

* Added Windows support.

* Added support for setting names on text objects.

* Added documentation.


# corpus 0.1.0

* First milestone, with support for JSON decoding, text segmentation,
  and text normalization.

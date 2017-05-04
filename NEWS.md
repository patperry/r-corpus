
# corpus 0.3.0

* Support for serializing dataset and text objects (readRDS/saveRDS).
  Unfortunately, this support doesn't come for free, and the objects
  take a little bit more memory.

* More convenient interface for accessing JSON arrays.

* Rename `as.text`/`is.text` to `as_text`/`is_text`; make `as_text`
  retain names, work on S3 objects.

* Add support for stemming via the Snowball library.

* Renames `read_json` to `read_ndjson` to not clash with `jsonlite`.


# corpus 0.2.0

* First CRAN release.

* Added Windows support.

* Added support for setting names on text objects.

* Added documentation.


# corpus 0.1.0

* First milestone, with support for JSON decoding, text segmentation,
  and text normalization.

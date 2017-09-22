To Do
=====

Bugs
----

 (no known bugs)


Features
--------

 * Update documentation for `text_tokens()`.

 * Add a `text_untoken()` function to turn token sequence into text:
   insert word-joiner (U+2060) to keep multi-word phrases together;
   put specified space character (ZWSP or SP) between tokens

 * Don't rely on abbreviations function in `text_filter.c`

 * Better warning/behavior if select contains dropped terms. Possibly ignore?

 * Expose `drop_space` and `drop_other` options for `text_filter`?

 * allow arbitrary function for stemmer

 * wrap.pad, width arguments to `utf8_print`

 * `token_kind` and `token_map` functions (?)

 * Add `ignore_dropped` option to `text_filter` (?)

 * Add demonstration of dictionary scaling with `text_match`:

       m <- text_match(x, dict$term)
       score <- tapply(dict$score[m$term], m$text, mean, default = 0)

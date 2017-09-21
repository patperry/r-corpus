To Do
=====

Bugs
----

 (no known bugs)


Features
--------

 * Better warning/behavior if select contains dropped terms. Possibly ignore?

 * Expose `drop_space` and `drop_other` options for `text_filter`?

 * allow arbitrary function for stemmer

 * wrap.pad, width arguments to `utf8_print`

 * `token_kind` and `token_map` functions (?)

 * Add `ignore_dropped` option to `text_filter` (?)

 * Add demonstration of dictionary scaling with `text_match`:

       m <- text_match(x, dict$term)
       score <- tapply(dict$score[m$term], m$text, mean, default = 0)

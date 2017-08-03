To Do
=====

Bugs
----

 (no known bugs)


Features
--------

 * `token_kind` and `token_map` functions (?)

 * `summary.text` could be better

 * Add `ignore_dropped` option to `text_filter` (?)

 * Add demonstration of dictionary scaling with `text_match`:

       m <- text_match(x, dict$term)
       score <- tapply(dict$score[m$term], m$text, mean, default = 0)

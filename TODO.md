To Do
=====

Bugs
----

 (no known bugs)


Features
--------

 * wrap.pad, width arguments to `utf8_print`

 * `token_kind` and `token_map` functions (?)

 * Add `ignore_dropped` option to `text_filter` (?)

 * Add demonstration of dictionary scaling with `text_match`:

       m <- text_match(x, dict$term)
       score <- tapply(dict$score[m$term], m$text, mean, default = 0)

Deprecated
----------
 * Remove 'random' argument from `text_locate`; make sure to remove the extra
   comma in `text_sample()`:

        loc <- text_locate(x, terms, filter,, ...)

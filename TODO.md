To Do
=====

Bugs
----

 (no known bugs)


Features
--------

 * Update documentation for `text_sub()`.

 * Add a `text_untoken()` function to turn token sequence into text:
   insert word-joiner (U+2060) to keep multi-word phrases together;
   put specified space character (ZWSP or SP) between tokens

 * wrap.pad, width arguments to `utf8_print`

 * `token_kind` and `token_map` functions (?)

 * Add demonstration of dictionary scaling with `text_match`:

       m <- text_match(x, dict$term)
       score <- tapply(dict$score[m$term], m$text, mean, default = 0)

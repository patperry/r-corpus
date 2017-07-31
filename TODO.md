To Do
=====

Bugs
----

 * `text_locate`, `text_split`, `term_frame` return text as a factor. This
   will throw a warning and give strange behavior if text names are not unique.

   Solution 1: Force text names to be unique.

   Solution 2: Always return text as an integer ID, not a factor.


Features
--------

 * `summary.text` could be better

 * Add `ignored_dropped` option to `text_filter`

 * `text_locate`, allows duplicates in the `term` argument. If the function
   disallowed this, then it could return the `term` column as a factor. This
   could make dictionary methods easier to apply:

       loc <- text_locate(x, dict$term)
       score <- c(tapply(dict$score[loc$term], loc$text, mean))

   Note: for this to work, the `text` column must be an integer ID or a
   factor.

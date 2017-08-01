To Do
=====

Features
--------

 * `summary.text` could be better

 * Add `ignored_dropped` option to `text_filter`

 * `text_locate`, allows duplicates in the `term` argument. If the function
   disallowed this, then it could return the `term` column as a factor. This
   could make dictionary methods easier to apply:

       loc <- text_locate(x, dict$term)
       score <- c(tapply(dict$score[loc$term], loc$text, mean))

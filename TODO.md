To Do
=====

Bugs
----

 (no known bugs)


Features
--------

 * `summary.text` could be better

 * Add `ignored_dropped` option to `text_filter`

 * `text_locate`, allows duplicates in the `term` argument. If the function
   disallowed this, then it could return the `term` column as a factor. This
   could make dictionary methods easier to apply:

       loc <- text_locate(x, dict$term)
       score <- tapply(dict$score[loc$term], loc$text, mean, default = 0)

  If term is a text, then instead the code is:

       loc <- text_locate(x, dict$term)
       score <- tapply(dict$score[match(loc$term, dict$term)],
                       loc$text, mean, default = 0)

  Another option is to introduce `text_match`, requiring term to be unique,
  but don't place the requirement on `text_{count,detect,locate}`. Then
  we can do

       m <- text_match(x, dict$term)
       score <- tapply(dict$score[m$term], m$text, mean, default = 0)
 
  or

      with(text_match(x, dict$term),
           tapply(dict$score[term], text, mean, default = 0))


  * rename `text_locate` columns: text, before, target, after

  * make `target` text, not character; include spaces in target

  * make `term_counts` return a `kind` or `class` column. Problem: kind
   (word break property) can change after nfkc normalization. Solution 1:
   only apply the nfkc maps that preserve normalization?
   Solution 2: report majority kind?

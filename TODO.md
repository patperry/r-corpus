To Do
=====

Features
--------

 * Concat operator `c()` for text.

 * `rep()` for text

 * `[[<-` for text

 * `[<-` for text

 * Need implementations for
   `names(.knownS3Generics)[.knownS3Generics == "base"]` for `text`.

 Math          
 Ops           
 Summary       
 Complex       
 as.data.frame 
 as.matrix     
 as.vector     
 cbind         
 labels        
 print         
 rbind         
 seq           
 solve         
 summary       
 t    

 * `summary.text`

 * `vocab()` for type/token frequencies (?)

 * `text_subset(,terms)`

 * Need calls to `R_CheckUserInterrupt()` in long-running computations
   (without leaking memory).

 * Passing a `token_filter` everywhere is annoying, error-prone. Allow
   setting a `filter` property on text objects to act as a default.

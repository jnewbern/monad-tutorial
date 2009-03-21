(module List scheme
  (require "Monad.ss")
  (provide bind return m-zero m-plus)
  
  ; ambiguous computation monad
  ; represent the results of an ambiguous computation
  ; as a list of possible results
  
  ; return a single value
  (define (return v) (list v))
  
  (define (bind mv f)
    (append-map f mv))
  
  ; return an empty ambiguous computation - no results
  (define m-zero '())
  
  ; take two ambiguous computations and declare the result
  ; as the result of one or the result of the other
  (define m-plus append)
  
  )

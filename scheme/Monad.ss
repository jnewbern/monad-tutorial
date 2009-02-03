; This module defines some syntax extensions and utilities for working with monads.
(module Monad scheme
  ; exported interface
  (provide letM)
  
  ; The letM macro is used to perform a single monadic binding.
  ;
  ;  (letM (name initializer) body) binds the monadic initializer expression to name
  ;  so that the value is used to resolve any reference to name in the monadic body expression.
  (define-syntax letM 
    (syntax-rules ()
      [(letM (name initializer) body) (bind initializer (lambda (name) body))]))

  ; end of module
  )
; This module defines some syntax extensions and utilities for working with monads.
(module Monad scheme
  ; exported interface
  (require mzlib/defmacro)

  (provide letM letM*)
  
  ; The letM macro is used to perform a single monadic binding.
  ;
  ;  (letM (name initializer) body) binds the monadic initializer expression to name
  ;  so that the value is used to resolve any reference to name in the monadic body expression.
  ;(define-syntax letM
  ;  (syntax-rules ()
  ;    [(letM (name initializer) body) (bind initializer (lambda (name) body))]))

  ;(define-syntax letM*
  ;  (syntax-rules ()
  ;    ((letM* () body) body)
  ;    ((letM* ((name1 mval1) (name2 mval2) ...)
  ;            body)
  ;     (letM (name1 mval1)
  ;           (letM* ((name2 mval2) ...)
  ;                  body)))))

    (define-macro letM
       (lambda (binding expr)
         (apply
          (lambda (name-val)
            (apply (lambda (name initializer)
                     `(bind ,initializer (lambda (,name) ,expr)))
                   name-val))
          binding)))

     (define-macro letM*
       (lambda (bindings expr)
         (if (and (pair? bindings) (pair? (cdr bindings)))
             `(letM ,(list (car bindings))
                    (letM* ,(cdr bindings) ,expr))
             `(letM ,bindings ,expr))))

  ; end of module
  )
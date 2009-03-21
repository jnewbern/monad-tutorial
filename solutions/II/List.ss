; This file describes an ambiguous computation monad 
; for PLT Scheme that we can use with the monad macros
; from the previous exercise

; If you're not using PLT Scheme, please translate it to your 
; favorite Lisp 

; After you've translated this list monad, evaluate the translated 
; equivalents of the following Scheme expressions:

; (return 5)

; (m-plus (return 5) (return 7))

; (m-plus (return 5) m-zero)

; (define (mul-m a b) (return (* a b))) 
; (define (double-m a) (mul-m a 2))

; (bind (return 5) double m)

; (bind m-zero double-m)

; (bind (m-plus (return 0) (return 3) (return 7)) double-m)

; (define (div-m a b) 
;    (if (= b 0) m-zero (return (/ a b))))
; (define (reciprocal-m a) (div-m 1 a))

; (bind (m-plus (return 0) (return 3) (return 7)) reciprocal-m)

; Can you explain the results of each evaluation?

(module List scheme
  (require "Monad.ss")
  (provide bind return m-zero m-plus)
  
  ; ambiguous computation monad
  ; represent an ambiguous computation as a list of possible results
  
  ; only one possible value - not ambiguous
  (define (return v) (list v))
  
  ; apply the ambiguous computation given by f
  ; to each of the possible previous values 
  ; and collect the possible results
  (define (bind mv f)
    (append-map f mv))
  
  ; Monad operations

  ; return an empty ambiguous computation
  (define m-zero '())
  
  ; take several ambiguous computations and join their possible results
  ; unlike Haskell's mplus, this one can take a variable number of arguments
  (define m-plus append)

  (define (displayn val) (display val) (newline))

  (define (run-tests)
    (define (mul-m a b) (return (* a b)))
    (define (double-m a) (mul-m a 2))
    (define (div-m a b)
      (if (= b 0) m-zero (return (/ a b))))
    (define (reciprocal-m a) (div-m 1 a))

    (displayn (return 5))
    (displayn (m-plus (return 5) (return 7)))
    (displayn (m-plus (return 5) m-zero))
    (displayn (bind (return 5) double-m))
    (displayn (bind m-zero double-m))
    (displayn (bind (m-plus (return 0) (return 3) (return 7)) double-m))
    (displayn (bind (m-plus (return 0) (return 3) (return 7)) reciprocal-m))

    )
  
    (run-tests)
  )


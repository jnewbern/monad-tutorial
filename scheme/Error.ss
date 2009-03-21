(module Error scheme
  (require "Monad.ss")
  (provide bind return fail inc-m div-m)
; error monad
; success is represented as '(ok val)
; failure is represented as '(fail failure-info)

; special operation is fail
(define (fail desc) `(fail ,desc))

(define (return val) `(ok ,val))

(define (bind mv f) 
  (if (eq? (car mv) 'ok)
      (f (cadr mv))
      mv))

; no explicit run function because we return things 
; in our desired representation directly
(define (inc-m v) (return (+ v 1)))

(define (div-m a b)
  (if (= b 0)
      (fail "division by zero")
      (return (/ a b))))

;> (bind (return 5) inc-m)
;(ok 6)
;> (bind (div-m 8 4) inc-m)
;(ok 3)
;> (bind (div-m 4 0) inc-m)
;(fail "division by zero")
;> 

;first letM* example
;> (letM* ((x (inc-m 3))
;        (y (div-m 8 2)))
;         (div-m 5 (- x y)))
;(fail "division by zero")

;second letM* example
;> (letM* ((x (inc-m 5))
;        (y (div-m 8 2)))
;       (let ((z (- x y)))
;         (div-m 2 z)))
;(ok 1)
)
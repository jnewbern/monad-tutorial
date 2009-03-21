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


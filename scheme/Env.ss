(module Env scheme
  (require "Monad.ss")
  (provide bind return run-with-env capture-env local-env)
  
  ; environment monad
  ; computations that depend on the values in an environment
  ; represented as (lambda (env) result)
  
  (define (return v) (lambda (env) v))
  
  (define (bind mv f)
    (lambda (env)
      ((f (mv env)) env)))

  ; run function to extract a result
  (define (run-with-env env mv) (mv env))

  ; capture the environment for reading
  (define capture-env
    (lambda (env) env))

  ; give a sub-computation a different local environment
  (define (local-env f mv)
    (lambda (env) (mv (f env))))
  
  )
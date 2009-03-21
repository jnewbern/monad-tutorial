(module Env scheme
  (require "Monad.ss")
  (provide bind return run-with-env capture-env lookup with-binding)
  
  ; environment monad
  ; computations that depend on the values in an environment
  ; represented as (lambda (env) result)
  
  (define (return v) (lambda (env) v))
  
  (define (bind mv f)
    (lambda (env)
      ((f (mv env)) env)))
  
  (define (run-with-env env mv) (mv env))
 
  (define capture-env
    (lambda (env) env))
  
  ; environments are represented as association lists
  (define (lookup name)
    (letM ((env capture-env))
          (return (cadr (assq name env)))))
  
  (define (with-binding name val mv)
    (lambda (env)
      (let* ((binding (list name val))
             (new-env (cons binding env)))
        (mv new-env))))
  
  )
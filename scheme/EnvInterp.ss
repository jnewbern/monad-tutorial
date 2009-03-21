(module EnvInterp scheme
  (require "Env.ss" "Monad.ss")
  
  (provide eval test-env)
         
  (define (eval exp env)
    (run-with-env env (analyze exp)))
  
  (define (analyze exp)
    (cond 
      ((number? exp) (return exp))
      ((symbol? exp) (lookup exp))
      ((pair? exp) (analyze-pair exp))))
  
  (define (analyze-function arg-name function-body)
    (let ((body (analyze function-body)))
      (letM ((env capture-env))
            (return
             ; our functions are procedures in the underlying Scheme
             (lambda (arg-val)
               ((with-binding arg-name arg-val body) env))))))

  (define (analyze-pair exp)
    (let ((tag (car exp)))
      ; (function <arg> body)
      (cond ((eq? 'function tag) (analyze-function (cadr exp) (caddr exp)))
            (else (analyze-app exp)))))
  
  (define (map-m mf vals)
    (if (null? vals)
        (return '())
        (letM* ((car-m (mf (car vals)))
                (cdr-m (map-m mf (cdr vals))))
               (return (cons car-m cdr-m)))))
  
  (define (analyze-app exp)
    (letM* ((fn (analyze (car exp)))
            (args (map-m analyze (cdr exp))))
           (begin 
             (return (apply fn args)))))
            
  (define test-env `((+ ,+) (- ,*) (a 5) (b 7)))
  
  (define (displayn val)
    (display val)
    (newline))
  
  (define (test-exp exp)
    (displayn `(eval ,exp test-env))    
    (display "> ")
    (displayn (eval exp test-env)))
  
  (define (test)
    (test-exp '5)
    (test-exp 'a)
    (test-exp '(+ 5 5))
    (test-exp '(+ 5 b))
    (test-exp '(- a b))
    (test-exp '((function n 92) a))
    (test-exp '((function a a) b))
    (test-exp '((function n (+ n a)) 23))
    (test-exp '((function b (- b a)) a))
    )
  
  (test)
  )
  
  
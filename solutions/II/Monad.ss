(module Monad scheme
  (require mzlib/defmacro)

  ; exported interface
  (provide letM letM*)

  ; Exercise: write macros for letM and letM* in your favorite Lisp
  ; base on the templates below

  ; letM should capture bind from the macro's use-environment 
  ; (in other words, letM should not be hygenic in bind) because 
  ; this file doesn't define bind
  ; bind is defined by the individual files
  ; where we implement particular monads, so we need
  ; to pick up bind based on the monad we're using at the moment
  ; 
  ; (letM ((name m-val)) expr) ==> 
  ;       (bind m-val (lambda (name) expr))

    (define-macro letM
       (lambda (binding expr)
         (apply
          (lambda (name-val)
            (apply (lambda (name initializer)
                     `(bind ,initializer (lambda (,name) ,expr)))
                   name-val))
          binding)))

  ; (letM* () expr) ==> expr
  ; (letM* (binding binding ...) expr) ==> 
  ;        (letM (binding) (letM* (binding ...) expr)

     (define-macro letM*
       (lambda (bindings expr)
         (if (and (pair? bindings) (pair? (cdr bindings)))
             `(letM ,(list (car bindings))
                    (letM* ,(cdr bindings) ,expr))
             `(letM ,bindings ,expr))))

  )
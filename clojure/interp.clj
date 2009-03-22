(ns monad-tutorial
  (:require [clojure.contrib.accumulators])
  (:use newmonads))

(defn trace [s x] (do (prn s x) x))

; ----------------------------------------------------------------------
; Interpreter scaffolding

; This function creates an interpreter from a set of predicates
; and actions in a given monad.
;
; It returns a function which takes an expression and tests it
; against each of the predicates.  When a match is found, the
; matching action is executed on the expression in the given monad.
; If no match is found, the supplied default-action is used.
; Predicates take an expression argument and return true or false.
; Actions take a monad, an interpreter function for recursion,
; and an expression and they return a value in the supplied monad.
;
; Arguments:
;   monad          - the monad structure to execute the interpeter in
;   parts          - a list of lists of (predicate,action) pairs to
;                    attempt to match the interpreted expression
;   default-action - an action to use if no match is found in parts
;
; Returns:
;   a function taking an expression to its interpeted form in the
;   given monad
(defn make-interp [monad parts default-action]
  (let [ part-list  (mapcat identity parts)
         check-part (fn [e part] ((first part) e))
         interp     (fn [rec e]
		      (let [ match  (seq (drop-while (complement (partial check-part e)) part-list))
			     action (if (nil? match)
				        default-action
				        (second (first match)))
			   ]
			(action monad (partial rec rec) e)))
	]
    (partial interp interp)))


; ----------------------------------------------------------------------
; Definitions of language fragments


; A simple arithmetic language supporting +,-,* and / of numeric literals.
(def arith-lang
  [ ; literal numbers
    [ (fn [e] (number? e))
    , (fn [m _ e] (domonad m [] e))
    ]
    ; binary addition operator
    [ (fn [e] (and (seq? e) (= (first e) '+)))
    , (fn [m rec e]
	(domonad m
		 [args (m-result (rest e))
		  x    (rec (first args))
		  y    (rec (second args))]
		 (+ x y)))
    ]
    ; binary subtraction operator
    [ (fn [e] (and (seq? e) (= (first e) '-)))
    , (fn [m rec e]
	(domonad m
		 [args (m-result (rest e))
		  x    (rec (first args))
		  y    (rec (second args))]
		 (- x y)))
    ]
    ; binary multiplication operator
    [ (fn [e] (and (seq? e) (= (first e) '*)))
    , (fn [m rec e]
	(domonad m
		 [args (m-result (rest e))
		  x    (rec (first args))
		  y    (rec (second args))]
		 (* x y)))
    ]
    ; division operator
    [ (fn [e] (and (seq? e) (= (first e) '/)))
    , (fn [m rec e]
	(domonad m
		 [args (m-result (rest e))
		  x    (rec (first args))
		  y    (rec (second args))
		  r    (if (and (number? y) (not= 0 y))
			 (m-result (/ x y))
			 (m-fail "division by 0"))]
		 r))
    ]
  ])

(defn lookup-in-env [m]
  (fn [name]
    (with-monad m
      (m-bind m-capture-env
        (fn [e] (m-result (second (find e name))))))))

(defn add-to-env [e k v] (assoc e k v))

; A language fragment supporting symbol lookup in an environment.
(def environment-lang
  [ ; symbol lookup
    [ (fn [e] (symbol? e))
    , (fn [m rec e]
	(domonad m
		 [v ((lookup-in-env m) e)
		  r (if v
		      (rec v)
		      (m-fail (str "undefined variable " e)))]
		 r))
    ]
  ])


; A language fragment supporting function abstraction

(defstruct closure :env :body)

(defn make-closure [env body] (struct closure env body))

(defn closure? [c] (and (map? c) (get c :env) (get c :body)))

(def fn-lang 
  [ ; When a closure is encountered, we interpret the body term in
    ; the supplied environment.
    [ (fn [e] (closure? e))
      (fn [m rec e]
	(with-monad m
	 (m-local-env (fn [_] (get e :env)) (rec (get e :body)))))
    ]
    ; When a function is encountered, (as from a lambda-v or lambda-n
    ; abstraction), it is lifted into the interpreter monad, where
    ; it is handled when it falls in the head position of an
    ; application term.
    [ (fn [e] (fn? e))
      (fn [m rec e] (domonad m [] e))
    ]
    ; a call-by-value lambda abstraction
    ; This captures the current environment and creates a function
    ; which, when applied to an argument-closure, inteprets it in
    ; the calling environment, adds the resulting bindings to
    ; the captured environment and evaluates the body term in the
    ; augmented environment.
    [ (fn [e] (and (seq? e) (= (first e) 'lambda-v)))
      (fn [m rec e]
	(domonad m
		 [args (m-result (rest e))
		  ce   m-capture-env]
		 (fn [arg_cl]
		   (domonad m
			    [arg_val (rec arg_cl)
			     new_env (m-result (add-to-env ce (first args) arg_val))
			     body_cl (m-result (make-closure new_env (second args)))
			     r       (rec body_cl)]
			    r))))
    ]
    ; a call-by-name lambda abstraction
    ; This captures the current environment and creates a function
    ; which, when applied to an argument-closure, binds it in the
    ; captured environment and evaluates the body term in the
    ; augmented environment.
    [ (fn [e] (and (seq? e) (= (first e) 'lambda-n)))
      (fn [m rec e]
	(domonad m
		 [args (m-result (rest e))
		  ce   m-capture-env]
		 (fn [arg_cl]
		   (let [new-env (add-to-env ce (first args) arg_cl)
			 body_cl (make-closure new-env (second args))]
		     (rec body_cl)))))
    ]
    ; When a sequence is found which has not matched anything yet,
    ; treat it as a function application.  Treat the first element
    ; as a function and create a closure for the argument.  Apply
    ; the function to the argument closure to get the result.
    [ (fn [e] (seq? e))
      (fn [m rec e]
	(domonad m
		 [f  (rec (first e))
		  ce m-capture-env
		  r  (if (fn? f)
		       (f (make-closure ce (second e)))
		       (m-fail (str "not a function: " f)))
		 ]
		 r))
    ]
  ])


; A language fragment for mutable reference cells

(defstruct interp-state :cells)

(def ref-lang 
  [ ; reference cell creation
    [ (fn [e] (and (seq? e) (= (first e) 'new-ref)))
      (fn [m rec e]
	(domonad m
		 [args (m-result (rest e))
		  val  (rec (first args))
		  s    m-get
		  refv (m-result (get s :cells))
		  idx  (m-result (count refv))
		  newv (m-result (conj refv val))
		  news (m-result (assoc s :cells newv))
		  _    (m-put news)]
		 idx))
    ]
    ; read a cell
    [ (fn [e] (and (seq? e) (= (first e) 'read)))
      (fn [m rec e]
	(domonad m
		 [args (m-result (rest e))
		  idx  (rec (first args))
		  s    m-get
		  refv (m-result (get s :cells))
		  v    (m-result (nth refv idx 'not-found))
		  x    (if (= v 'not-found)
			 (m-fail "invalid reference")
			 (rec v))]
		 x))
    ]
    ; write a cell
    [ (fn [e] (and (seq? e) (= (first e) 'write)))
      (fn [m rec e]
	(domonad m
		 [args (m-result (rest e))
		  idx  (rec (first args))
		  val  (rec (second args))
		  s    m-get
		  refv (m-result (get s :cells))
		  newv (m-result (assoc refv idx val))
		  news (m-result (assoc s :cells newv))
		  _    (m-put news)]
		 nil))
    ]
  ])


; A language fragment for continuations

(def cont-lang 
  [ ; handle "default" call-cc form (use outermost effect)
    [ (fn [e] (and (seq? e) (= (first e) 'call-cc)))
      (fn [m rec e]
	(domonad m
		 [args (m-result (rest e))
		  ce   m-capture-env
		  r    (m-call-cc
			(fn [cont]
			  (let [new-cont (fn [arg] (domonad m
							    [v (rec arg)
							     r (cont v)]
							    r))
				new-env  (add-to-env ce (first args) new-cont)
				body_cl  (make-closure new-env (second args))]
			    (rec body_cl))))]
		 r))
    ]
  ])

(defn alt-cont-lang [alt-call-cc-fn]
  [
    ; handle alternative call-cc (using a special effect function)
    [ (fn [e] (and (seq? e) (= (first e) 'alt-call-cc)))
      (fn [m rec e]
	(domonad m
		 [args (m-result (rest e))
		  ce   m-capture-env
		  r    (alt-call-cc-fn
			(fn [cont]
			  (let [new-cont (fn [arg] (domonad m
							    [v (rec arg)
							     r (cont v)]
							    r))
				new-env  (add-to-env ce (first args) new-cont)
				body_cl (make-closure new-env (second args))]
			    (rec body_cl))))]
		 r))
    ]
  ])


; A language fragment for the "do" form

(def do-lang 
  [ ; handle do sequence
    [ (fn [e] (and (seq? e) (= (first e) 'do)))
      (fn [m rec e]
	(let [args (rest e)]
	  (if (empty? args)
	    (with-monad m (m-fail "nothing to do"))
	    (let [to_do (map rec args)]
	      (domonad m [results (m-seq to_do)] (last results))))))
    ]
  ])

; ----------------------------------------------------------------------
; Tests

(defn do-test [ifn name arg expected]
  (let [result (ifn arg)]
    (if (= result expected)
      (do (println (str "PASSED: " name))
	  (println (str "  " (pr-str arg) " ==> " (pr-str result))))
      (do (println (str "FAILED: " name))
	  (println (str "  " (pr-str arg) " ==> " (pr-str result)))
	  (println (str "   but expected " (pr-str expected)))))))

; Create an interpreter using just the arithmetic language fragment
; and run a few test expressions.

(defn fail-bad-token [m _ e]
  (with-monad m (m-fail (str "bad-token at " (pr-str e)))))

(def arith-interp (make-interp error-m [arith-lang] fail-bad-token))

(defn do-arith-tests []
  (do-test arith-interp "plus"
	   '(+ 3 1)
	   '(ok 4))
  (do-test arith-interp "div-0"
	   '(/ (+ 4 1) (- 12 (* 3 4)))
	   '(fail "division by 0"))
  (do-test arith-interp "bad-foo"
	   '(/ (+ 4 1) (- 12 (foo 3 4)))
	   '(fail "bad-token at (foo 3 4)"))
  )

; Create an interpreter using the arithmetic language fragment and
; an environment of symbols and run a few tests.
(defn arith-env-interp [symbol-environment]
     (let [ monad     (env-t error-m)
            parts     [ arith-lang
		      , environment-lang
		      ]
	    otherwise fail-bad-token
	    interp    (make-interp monad parts otherwise)
	  ]
       (fn [e] (run-with-env symbol-environment (interp e)))))

(defn do-arith-env-tests []
  (do-test (arith-env-interp {'pi 3.14159}) "2-pi"
	   '(* 2 pi)
	   '(ok 6.28318))
  (do-test (arith-env-interp {'x 42, 'y 6}) "div-vars"
	   '(/ x y)
	   '(ok 7))
  (do-test (arith-env-interp {'x 7, 'y 21}) "div-0-expr"
	   '(/ 110 (- y (* 3 x)))
	   '(fail "division by 0"))
  )

; Create an interpreter using the arithmetic language fragment,
; "do" sequences & reference cells and run a few tests.
(def arith-ref-do-interp
     (let [ monad     (state-t error-m)
            parts     [ arith-lang
		      , ref-lang
		      , do-lang
 		      ]
	    otherwise fail-bad-token
	    interp    (make-interp monad parts otherwise)
	  ]
       (fn [e]
	 (let [initial-state (struct interp-state (vector 0))] ; one pre-defined reference
	   ((eval-state-t error-m) (interp e) initial-state)))))

(defn do-arith-ref-tests []
  (do-test arith-ref-do-interp "make-ref"
	   '(new-ref 7)
	   '(ok 1))
  (do-test arith-ref-do-interp "read-ref"
	   '(read (new-ref (+ 3 4)))
	   '(ok 7))
  (do-test arith-ref-do-interp "bad-foo"
	   '(+ 4 (foo 7))
	   '(fail "bad-token at (foo 7)"))
  (do-test arith-ref-do-interp "bad-ref"
	   '(read 1)
	   '(fail "invalid reference"))
  (do-test arith-ref-do-interp "bad-do"
	   '(do)
	   '(fail "nothing to do"))
  (do-test arith-ref-do-interp "do-one"
	   '(do (* 7 2))
	   '(ok 14))
  (do-test arith-ref-do-interp "do-two"
	   '(do (* 7 2) (* 7 3))
	   '(ok 21))
  )

; Create an interpreter with arithmetic, env+functions, do & reference
; cells and run a few tests.

; we will use a (state-t (env-t error-m)) stack, abbreviated st-env-err
(def st-env-err (state-t (env-t error-m)))

(defn arith-ref-do-fn-interp [initial-environment]
     (let [ monad     st-env-err
            parts     [ arith-lang
		      , environment-lang
		      , ref-lang
		      , do-lang
		      , fn-lang
 		      ]
	    otherwise fail-bad-token
	    interp    (make-interp monad parts otherwise)
	  ]
       (fn [e]
	 (let [initial-state (struct interp-state (vector 0)) ; one pre-defined reference
	       eval-state-fn (eval-state-t (env-t error-m))
	       ev            (eval-state-fn (interp e) initial-state)]
	   (run-with-env initial-environment ev)))))

(defn do-arith-ref-do-tests []

  (do-test (arith-ref-do-fn-interp {}) "mult"
	   '(* 3 3)
	   '(ok 9))
  
  (do-test (arith-ref-do-fn-interp {'y 4}) "square"
	   '((lambda-v x (* x x)) 3)
	   '(ok 9))
  
  (do-test (arith-ref-do-fn-interp {}) "bad-fn"
	   '((+ 3 6) 7)
	   '(fail "not a function: 9"))
  
  (do-test (arith-ref-do-fn-interp {'x 7, 'y 21}) "cbv"
	   '(+ x ((lambda-v x (* x x)) (- y x)))
	   '(ok 203))
  
  (do-test (arith-ref-do-fn-interp {'x 7, 'y 21}) "cbn"
	   '(+ x ((lambda-n x (* x x)) (- y x)))
	   '(ok 203))
  
  (do-test (arith-ref-do-fn-interp {}) "cbv-failure"
	   '((lambda-v x 5) (/ 5 0))
	   '(fail "division by 0"))
  
  (do-test (arith-ref-do-fn-interp {}) "cbn-success"
	   '((lambda-n x 5) (/ 5 0))
	   '(ok 5))

  (do-test (arith-ref-do-fn-interp {}) "make-ref-2"
	   '(new-ref 7)
	   '(ok 1))

  (do-test (arith-ref-do-fn-interp {}) "read-ref-2"
	   '(read (new-ref (+ 3 4)))
	   '(ok 7))

  (do-test (arith-ref-do-fn-interp {}) "bad-foo-2"
	   '(+ 4 (foo 7))
	   '(fail "undefined variable foo"))
  
  (do-test (arith-ref-do-fn-interp {}) "bad-ref-2"
	   '(read 1)
	   '(fail "invalid reference"))
  
  (do-test (arith-ref-do-fn-interp {}) "bad-do-2"
	   '(do)
	   '(fail "nothing to do"))

  (do-test (arith-ref-do-fn-interp {}) "do-one-2"
	   '(do (* 7 2))
	   '(ok 14))

  (do-test (arith-ref-do-fn-interp {}) "do-two-2"
	   '(do (* 7 2) (* 7 3))
	   '(ok 21))

  (do-test (arith-ref-do-fn-interp {}) "cbv-refs"
	   '((lambda-v r (+ 1 (read r))) (new-ref 7))
	   '(ok 8))

  (do-test (arith-ref-do-fn-interp {}) "incr-3"
	   '((lambda-v x-ref
		       ((lambda-n f (do f f f)) (do (write x-ref (+ 1 (read x-ref)))
						  (read x-ref))))
	     (new-ref 3))
	   '(ok 6))

  (do-test (arith-ref-do-fn-interp {}) "incr-and-sum"
	   '(((lambda-v x-ref
			(lambda-v y-ref
				  ((lambda-n inc-and-sum (* inc-and-sum (+ inc-and-sum inc-and-sum)))
				   (do (write x-ref (+ 1 (read x-ref)))
				       (write y-ref (+ 1 (read y-ref)))
				       (+ (read x-ref) (read y-ref))))))
	      (new-ref 3))
	     (new-ref 5))
	   '(ok 260))
  )

; use the pre-defined reference as time counter
(def interp-with-time-ref
     (arith-ref-do-fn-interp {'time  0, ; using the pre-defined reference
			      'tick '(write time (+ 1 (read time))) }))

(defn do-interp-with-time-tests []

  (do-test interp-with-time-ref "start-time"
	   '(read time)
	   '(ok 0))

  (do-test interp-with-time-ref "incr-time"
	   '(do tick (read time))
	   '(ok 1))

  (do-test interp-with-time-ref "add-time"
	   '(+ (do tick 1) (read time))
	   '(ok 2))
  )

; Create an interpreter with arithmetic, env+functions, do,
; continuations & reference cells and run a few tests.

; we will use an (env-t (state-t (cont-t error-m))) stack, abbreviated ecsce
(def ecsce (env-t (cont-t (state-t (cont-t error-m)))))

; these parts of the stack are used for lifting
(def csce (cont-t (state-t (cont-t error-m))))
(def sce (state-t (cont-t error-m)))

(def ecsce-alt-call-cc
     (with-monad ecsce
      (lift-call-cc
       (with-monad csce
        (lift-call-cc
	 (with-monad sce m-call-cc)
	 m-result m-bind m-base t-base t-map))
       m-result m-bind m-base t-base t-map)))

(defn arith-ref-do-fn-cont-interp [initial-environment]
     (let [ monad     ecsce
            parts     [ arith-lang
		      , environment-lang
		      , ref-lang
		      , do-lang
		      , cont-lang
		      , (alt-cont-lang ecsce-alt-call-cc)
		      , fn-lang
 		      ]
	    otherwise fail-bad-token
	    interp    (make-interp monad parts otherwise)
	  ]
       (fn [e]
	 (let [eval-env (partial run-with-env initial-environment)
	       v0 (eval-env (interp e))
	       eval-cont-fn1 (fn [x] (eval-cont-t (state-t (cont-t error-m)) x))
	       v1 (eval-cont-fn1 v0)
	       initial-state (struct interp-state (vector 0)) ; with cell 0 pre-defined
	       eval-state-fn (eval-state-t (cont-t error-m))
	       v2            (eval-state-fn v1 initial-state)
	       eval-cont-fn2  (fn [x] (eval-cont-t error-m x))
	       v3            (eval-cont-fn2 v2)
	      ]
	   v3))))

(defn do-ref-do-fn-cont-tests []

  (do-test (arith-ref-do-fn-cont-interp {}) "mult-2"
	   '(* 3 3)
	   '(ok 9))
  
  (do-test (arith-ref-do-fn-cont-interp {'pi 3.14159}) "env"
	   '(* 2 pi)
	   '(ok 6.28318))
  
  (do-test (arith-ref-do-fn-cont-interp {}) "square-2"
	   '((lambda-v x (* x x)) 3)
	   '(ok 9))
  
  (do-test (arith-ref-do-fn-cont-interp {}) "bad-fn-2"
	   '((+ 3 6) 7)
	   '(fail "not a function: 9"))
  
  (do-test (arith-ref-do-fn-cont-interp {'x 7, 'y 21}) "cbv-2"
	   '(+ x ((lambda-v x (* x x)) (- y x)))
	   '(ok 203))
  
  (do-test (arith-ref-do-fn-cont-interp {'x 7, 'y 21}) "cbn-2"
	   '(+ x ((lambda-n x (* x x)) (- y x)))
	   '(ok 203))
  
  (do-test (arith-ref-do-fn-cont-interp {}) "cbv-failure-2"
	   '((lambda-v x 5) (/ 5 0))
	   '(fail "division by 0"))
  
  (do-test (arith-ref-do-fn-cont-interp {}) "cbn-success-2"
	   '((lambda-n x 5) (/ 5 0))
	   '(ok 5))
  
  (do-test (arith-ref-do-fn-cont-interp {}) "make-ref-3"
	   '(new-ref 7)
	   '(ok 1))
  
  (do-test (arith-ref-do-fn-cont-interp {}) "read-ref-3"
	   '(read (new-ref (+ 3 4)))
	   '(ok 7))
  
  (do-test (arith-ref-do-fn-cont-interp {}) "bad-foo-3"
	   '(+ 4 (foo 7))
	   '(fail "undefined variable foo"))

  (do-test (arith-ref-do-fn-cont-interp {}) "bad-ref-3"
	   '(read 1)
	   '(fail "invalid reference"))

  (do-test (arith-ref-do-fn-cont-interp {}) "bad-do-3"
	   '(do)
	   '(fail "nothing to do"))

  (do-test (arith-ref-do-fn-cont-interp {}) "do-one-3"
	   '(do (* 7 2))
	   '(ok 14))

  (do-test (arith-ref-do-fn-cont-interp {}) "do-two-3"
	   '(do (* 7 2) (* 7 3))
	   '(ok 21))

  (do-test (arith-ref-do-fn-cont-interp {}) "cbv-refs-2"
	   '((lambda-v r (+ 1 (read r))) (new-ref 7))
	   '(ok 8))

  (do-test (arith-ref-do-fn-cont-interp {}) "incr-by-3"
	   '((lambda-v x-ref
		       ((lambda-n f (do f f f)) (do (write x-ref (+ 1 (read x-ref)))
						    (read x-ref))))
	     (new-ref 3))
	   '(ok 6))
  
  (do-test (arith-ref-do-fn-cont-interp {}) "incr-and-sum-2"
	   '(((lambda-v x-ref
			(lambda-v y-ref
				  ((lambda-n inc-and-sum (* inc-and-sum (+ inc-and-sum inc-and-sum)))
				   (do (write x-ref (+ 1 (read x-ref)))
				       (write y-ref (+ 1 (read y-ref)))
				       (+ (read x-ref) (read y-ref))))))
	      (new-ref 3))
	     (new-ref 5))
	   '(ok 260))
  )
  
; use the pre-defined reference as time counter
(def interp-with-time-ref-ecsce
     (arith-ref-do-fn-cont-interp {'time  0, ; using the pre-defined reference
			           'tick '(write time (+ 1 (read time))) }))

(defn do-interp-with-time-ref-tests []

  (do-test interp-with-time-ref-ecsce "start-time-2"
	   '(read time)
	   '(ok 0))
  
  (do-test interp-with-time-ref-ecsce "incr-time-2"
	   '(do tick (read time))
	   '(ok 1))
  
  (do-test interp-with-time-ref-ecsce "add-time-2"
	   '(+ (do tick 1) (read time))
	   '(ok 2))
  
  (do-test (arith-ref-do-fn-cont-interp {}) "call-cc"
	   '(call-cc exit (+ 5 (exit 3)))
	   '(ok 3))
  
  (do-test (arith-ref-do-fn-cont-interp {}) "call-cc-2"
	   '(+ (call-cc exit (+ 7 (exit 9)))
	       (+ 2 (call-cc exit (- 2 (exit 3)))))
	   '(ok 14))
  
  (do-test (arith-ref-do-fn-cont-interp {}) "call-cc-div"
	   '(* (call-cc exit (/ (exit 5) 0)) 5)
	   '(ok 25))
  
  (do-test (arith-ref-do-fn-cont-interp {}) "call-cc-fn"
	   '((call-cc exit-fn (lambda-v n (* n 2))) 5)
	   '(ok 10))
  
  (do-test (arith-ref-do-fn-cont-interp {}) "call-cc-escape"
	   '((call-cc exit-fn
		      (lambda-v n
				(+ n
				   (exit-fn (lambda-v r (* n (+ r 1))))))) 5)
	   '(ok 30))
  
  (do-test interp-with-time-ref-ecsce "alt-call-cc-resets-state"
	   '(do tick
		(+ (alt-call-cc exit
				(do tick
				    tick
				    ((lambda-v t0 (exit t0)) (read time))))
		   (read time)))
	   '(ok 4))
  
  (do-test interp-with-time-ref-ecsce "call-cc-preserves-state"
	   '(do tick
		(+ (call-cc exit
			    (do tick
				tick
				((lambda-v t0 (exit t0)) (read time))))
		   (read time)))
	   '(ok 6))
  )

; a language fragment that accumulates a log of numbers in a string
; we also provide the ability to print them
(defn write-number [m v]
  (with-monad m
    (if (number? v)
      (domonad
       [_ (m-write (str v))
	_ (m-write " ")]
       nil)
      (m-fail (str "attempt to write non-number: " v)))))

; our log is just a string
(def empty-log clojure.contrib.accumulators/empty-string)

(def log-lang
  [ ; log a number
   [ (fn [e]
       (and (seq? e) (= (first e) 'write-nums)))
     (fn [m rec e]
       (domonad m
	 [args (m-result (rest e))
	  vals (m-map rec args)
          _    (m-seq (map (partial write-number m) vals))]
	 nil)) ]
   ,
   ; collect and print the numbers logged while evaluating
   ; the list of argument expressions
   ; return the last expression as the result
   [ (fn [e] (and (seq? e) (= (first e) 'collect-nums)))
     (fn [m rec e]
       (domonad m
         [args (m-result (rest e))
	  [vals log] (m-listen (m-map rec args))
	  _ (m-result (println (str "number log: " log)))
	  ]
	 (last vals))) ]
   ,
   ; censor any values logged while evaluating its argument
   [ (fn [e] (and (seq? e) (= (first e) 'censor-nums)))
     (fn [m rec e]
       (with-monad m
	 (m-censor (constantly empty-log) (rec (second e)))))
   ]
  ])

(def arith-log-interp
  (let [ monad     (writer-t empty-log error-m)
	 parts     [ arith-lang, log-lang, do-lang ]
	 otherwise fail-bad-token
	 interp    (make-interp monad parts otherwise)
       ]
    ; we don't eval-writer-t because we want to see the final log
    (fn [e] (interp e))))

(defn do-arith-log-tests []
  (let [test (partial do-test arith-log-interp)]
    (test "log-one" '(write-nums 5) '(ok [nil "5 "]))
    (test "log-two" '(do (write-nums 16) (write-nums (+ 5 7)))
	  '(ok [nil "16 12 "]))
    (test "censor-one" '(censor-nums (write-nums 19)) '(ok [nil ""]))
    (test "censor-two"
	  '(do (censor-nums (write-nums (+ 7 9)))
	       (write-nums (* 8 12)))
	  '(ok [nil "96 "]))
    (println "\nlisten-one should print: " "number log: 17 23 ")
    (test "listen-one"
	  '(censor-nums
	    (collect-nums
	     (write-nums 17 23)))
	  '(ok [nil ""]))
    (println "\nlisten-two should print the number log and return it")
    (test "listen-two"
	  '(collect-nums
	    (write-nums (+ 17 (do (write-nums 12) 23)))
	    (censor-nums (do (write-nums 99) 41)))
	  '(ok [41 "12 40 "]))

  ))

(defn do-tests []
  (do-arith-tests)
  (do-arith-env-tests)
  (do-arith-ref-tests)
  (do-arith-ref-do-tests)
  (do-interp-with-time-tests)
  (do-ref-do-fn-cont-tests)
  (do-interp-with-time-ref-tests)
  (do-arith-log-tests)
  )

  
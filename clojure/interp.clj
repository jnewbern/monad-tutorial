(ns monad-tutorial
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
		      (let [ match  (drop-while (complement (partial check-part e)) part-list)
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
  [ ; a call-by-value lambda abstraction
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
    ; When a closure is encountered, we interpret the body term in
    ; the supplied environment.
    [ (fn [e] (closure? e))
      (fn [m rec e]
	(with-monad m
	 (m-local-env (fn [_] (get e :env)) (rec (get e :body)))))
    ]
    ; When a function is encountered, (as from a lambda-v or lambda-n
    ; abstraction), it is lifted into the interpreter monad, where
    ; it presumably ends up being handled by the function below.
    [ (fn [e] (fn? e))
    , (fn [m rec e] (domonad m [] e))
    ]
    ; When a sequence is found which has not matched anything yet,
    ; treat it as a function application.  Treat the first element
    ; as a function and create a closure for the argument.  Apply
    ; the function to the argument closure to get the result.
    [ (fn [e] (seq? e))
      (fn [m rec e]
	(domonad m
		 [f (rec (first e))
		  ce m-capture-env
		  r (f (make-closure ce (second e)))]
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

(do-test arith-interp "plus"
	 '(+ 3 1)
	 '(ok 4))
(do-test arith-interp "div-0"
	 '(/ (+ 4 1) (- 12 (* 3 4)))
	 '(fail "division by 0"))
(do-test arith-interp "bad-foo"
	 '(/ (+ 4 1) (- 12 (foo 3 4)))
	 '(fail "bad-token at (foo 3 4)"))

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

(do-test (arith-env-interp {'pi 3.14159}) "2-pi"
	 '(* 2 pi)
	 '(ok 6.28318))
(do-test (arith-env-interp {'x 42, 'y 6}) "div-vars"
	 '(/ x y)
	 '(ok 7))
(do-test (arith-env-interp {'x 7, 'y 21}) "div-0-expr"
	 '(/ 110 (- y (* 3 x)))
	 '(fail "division by 0"))


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
	 (let [initial-state (struct interp-state (vector 0))]
	   ((eval-state-t error-m) (interp e) initial-state)))))

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
	 (let [initial-state (struct interp-state (vector 0))
	       eval-state-fn (eval-state-t (env-t error-m))
	       ev            (eval-state-fn (interp e) initial-state)]
	   (run-with-env initial-environment ev)))))

(do-test (arith-ref-do-fn-interp {}) "square"
	 '((lambda-v x (* x x)) 3)
	 '(ok 9))

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

; ----------------------------------------------------------------------
; old interpreter

(def interp-monad (cont-t (state-t (cont-t (env-t error-m)))))

(def interp-call-cc
     (with-monad interp-monad (lift-call-cc m-result m-bind m-base t-base ::undefined)))

(def interp-alt-call-cc (with-monad interp-monad m-call-cc))

; interpreter environments
(def interp-lookup (lookup-in-env interp-monad))

; interp is a hack to fake top-level recursion
(defn interp-closure [c interp]
  (with-monad interp-monad
    (m-local-env (fn [_] (get c :env)) (interp (get c :body)))))

; wrap continuation into usable function
; interp hack again
(defn interp-wrap-cont [interp cont]
  (fn [arg] (domonad interp-monad
              [v (interp arg)
               r (cont v)]
              r)))

(defn interp [e]
;  (prn "interp: " e)
  (cond
   (symbol? e) (domonad interp-monad
		  [v (interp-lookup e)
		   r (if v
		         (interp v)
			 (m-fail (str "undefined variable " e)))]
		  r)
   (number? e) (domonad interp-monad [] e)
   (closure? e) (interp-closure e interp)
   (fn? e) (domonad interp-monad [] e)
   (seq? e)    (let [t    (first e)
		     args (rest e)]
		 (cond
		  (= t '+)        (domonad interp-monad
				     [x (interp (first args))
				      y (interp (second args))]
				     (+ x y))
		  (= t '-)        (domonad interp-monad
				     [x (interp (first args))
				      y (interp (second args))]
				     (- x y))
		  (= t '*)        (domonad interp-monad
				     [x (interp (first args))
				      y (interp (second args))]
				     (* x y))
		  (= t '/)        (domonad interp-monad
				     [x (interp (first args))
				      y (interp (second args))
				      r (if (and (number? y) (not= 0 y))
					    (m-result (/ x y))
					    (m-fail "division by 0"))]
				     r)
		  (= t 'lambda-v) (domonad interp-monad
				     [ce m-capture-env]
				     (fn [arg_cl] (domonad interp-monad
						     [arg_val (interp arg_cl)
                                                      new_env (m-result (add-to-env ce (first args) arg_val))
						      body_cl (m-result (make-closure new_env (second args)))
						      r       (interp body_cl)]
						     r)))
		  (= t 'lambda-n) (domonad interp-monad
				     [ce m-capture-env]
				     (fn [arg_cl]
                                       (let [new-env (add-to-env ce (first args) arg_cl)
                                             body_cl (make-closure new-env (second args))]
                                            (interp body_cl))))
		  (= t 'new-ref)  (domonad interp-monad
				     [val  (interp (first args))
				      s    m-get
				      refv (m-result (get s :cells))
				      idx  (m-result (count refv))
				      newv (m-result (conj refv val))
				      news (m-result (assoc s :cells newv))
				      _    (m-put news)]
				     idx)
		  (= t 'read)     (domonad interp-monad
				     [idx  (interp (first args))
				      s    m-get
				      refv (m-result (get s :cells))
				      v    (m-result (nth refv idx 'not-found))
				      x    (if (= v 'not-found)
					     (m-fail "invalid reference")
					     (interp v))]
				     x)
		  (= t 'write)    (domonad interp-monad
				     [idx  (interp (first args))
				      val  (interp (second args))
				      s    m-get
				      refv (m-result (get s :cells))
				      newv (m-result (assoc refv idx val))
				      news (m-result (assoc s :cells newv))
				      _    (m-put news)]
				     nil)
                  (= t 'call-cc) (domonad interp-monad
                                  [ce m-capture-env
                                   r (interp-call-cc
                                       (fn [cont]
                                         (let [new-cont (interp-wrap-cont interp cont)
                                               new-env (add-to-env ce (first args) new-cont)
                                               body_cl (make-closure new-env (second args))]
                                              (interp body_cl))))]
                                   r)
                  (= t 'alt-call-cc) (domonad interp-monad
                                      [ce m-capture-env
                                       r (interp-alt-call-cc
                                           (fn [cont]
                                             (let [new-cont (interp-wrap-cont interp cont)
                                                   new-env  (add-to-env ce (first args) new-cont)
                                                   body_cl (make-closure new-env (second args))]
                                               (interp body_cl))))]
                                       r)
                  (= t 'do) (if (empty? args)
                                (with-monad interp-monad (m-fail "nothing to do"))
                                (let [to_do (map interp args)]
                                     (domonad interp-monad
                                        [results (m-seq to_do)]
                                        (last results))))
		  :default (domonad interp-monad
                             [f (interp (first e))
                              ce m-capture-env
                              r (f (make-closure ce (second e)))]
                             r)
		  ))
   ))

; examples

(def initial-env {'x     7,
		  'y    21,
                  'time  0, ; time is a pre-defined reference
		  'tick '(write time (+ 1 (read time)))
                  })

(def initial-state (struct interp-state (vector 0)))

(def run-interp-state (eval-state-t (cont-t (env-t error-m))))

(defn run-interp [exp]
      (prn 'run-interp exp)
      (prn (run-with-env initial-env
             (eval-cont-t (env-t error-m)
               (run-interp-state 
		(eval-cont-t (state-t (cont-t (env-t error-m)))
			     (interp exp))
		initial-state)))))

; success: 3
(run-interp 3)

; success: 4
(run-interp '(+ 2 2))

; success: 7
(run-interp 'x)

;fail: undefined variable: z
(run-interp 'z)

; success: 17
(run-interp '(+ 3 (* 2 x)))

;fail: undefined variable: z
(run-interp '(+ 4 z))

; success 4
(run-interp '(/ 12 3))

; fail - division-by 0
(run-interp '(/ 12 (- 21 y)))

; success: 9
(run-interp  '((lambda-v x (* x x)) 3))

; success: 203
(run-interp  '(+ x ((lambda-v x (* x x)) (- y x))))

; success: 203
(run-interp  '(+ x ((lambda-n x (* x x)) (- y x))))

; fail - division by 0
(run-interp '((lambda-v x 5) (/ 5 0)))

; success 5
(run-interp '((lambda-n x 5) (/ 5 0)))

; success 3
(run-interp '(call-cc exit (+ 5 (exit 3))))

; success 14
(run-interp '(+ (call-cc exit (+ 7 (exit 9)))
                (+ 2 (call-cc exit (- 2 (exit 3))))))

; success 25
(run-interp '(* (call-cc exit (/ (exit 5) 0))
               5))

; success 10
(run-interp '((call-cc exit-fn (lambda-v n (* n 2))) 5))

; continuation escaping call-cc
; success 30
(run-interp '((call-cc exit-fn
                (lambda-v n
                   (+ n
                     (exit-fn (lambda-v r (* n (+ r 1))))))) 5))



; fail - nothing to do
(run-interp '(do))

; success 7
(run-interp '(do 5 7))

; success 0
(run-interp '(read time))

; success 1
(run-interp '(do tick (read time)))

; success 2
(run-interp '(+ (do tick 1) (read time)))

; demonstrate call-cc resets the state
; success 4 (not 6)
(run-interp
  '(do tick
       (+ (call-cc exit
            (do tick
                tick
                ((lambda-v t0 (exit t0)) (read time))))
           (read time))))

; demonstrate alt-call-cc preserves the state
; success 6 (not 4)
(run-interp
  '(do tick
       (+ (alt-call-cc exit
            (do tick
                tick
                ((lambda-v t0 (exit t0)) (read time))))
           (read time))))



; success 1
(run-interp '(new-ref 7))

; success 7
(run-interp '(read (new-ref 7)))

; success 8
(run-interp '((lambda-v r (+ 1 (read r))) (new-ref 7)))

; success 6
(run-interp
  '((lambda-v x-ref
      ((lambda-n f (do f f f)) (do (write x-ref (+ 1 (read x-ref)))
				   (read x-ref))))
    (new-ref 3)))

; success 260
(run-interp
   '(((lambda-v x-ref
        (lambda-v y-ref
	   ((lambda-n inc-and-sum (* inc-and-sum (+ inc-and-sum inc-and-sum)))
	    (do (write x-ref (+ 1 (read x-ref)))
		(write y-ref (+ 1 (read y-ref)))
		(+ (read x-ref) (read y-ref))))))
      (new-ref 3))
     (new-ref 5)))

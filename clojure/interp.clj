(ns monad-tutorial
  (:use clojure.contrib.monads))

(defn trace [s x] (do (prn s x) x))

; error monad

(defmonad error
  "Monad describing computations with possible failures.
   Values in the monad pair a status code with either a value
   or a failure descriptor."
  [m-result  (fn m-result-error [v]  (list 'ok v))
   m-bind    (fn m-bind-error [mv f] (if (= 'ok (first mv))
				         (f (second mv))
					 mv))
   ])

(defn fail [err] (list 'fail err))

(defn success? [mv] (= 'ok (first mv)))
(defn failure? [mv] (not (success? mv)))

(defn successful-value [mv] (if (success? mv) (second mv) 'nil))
(defn error-desc       [mv] (if (failure? mv) (second mv) 'nil))

; environment monad

(defmonad env
  "Monad which allows a computation to access
   values from an environment"
  [m-result (fn m-result-env [v]
	      (fn [_] v))
   m-bind   (fn m-bind-env [mv f]
	      (fn [e] ((f (mv e)) e)))
   ])

(defn capture-env [e] e)

(defn ask-env [k]
  (fn [e] (second (find e k))))

(defn local-env [f mv]
  (fn [e] (mv (f e))))

(defn run-with-env [e mv]
  (mv e))

(defn env-t
  "Monad transformer that adds an environment to an existing monad"
  [m]
  (monad [m-result (with-monad m
		     (fn m-result-env-t [v]
		       (fn [_] (m-result v))))
	  m-bind   (with-monad m
		     (fn m-bind-env-t [mv f]
		       (fn [e] (m-bind (run-with-env e mv)
				       (fn [x] ((f x) e))))))]))

(defn capture-env-t [m]
  (fn [e] (with-monad m (m-result e))))

(defn lift-env-t [mv] (fn [_] mv))

(defn ask-env-t [m k]
  (domonad (env-t m) [e (capture-env-t m)] (second (find e k))))

; continuation monad transformer
; in a dynamically typed setting this seems to also be
; the continuation monad itself
(defn cont-t
  "Monad transformer that adds continuations to an existing monad"
  [m]
  (monad [m-result (fn m-result-cont-t [v]
                      (fn [k] (k v)))
          m-bind   (fn m-bind-cont-t [mv f]
                     (fn [c] (mv (fn [a] ((f a) c)))))]))

(defn lift-cont-t [m]
   (fn [mv] (with-monad m (partial m-bind mv))))

(defn callcc [f] (fn [c] (f (fn [a] (fn [_] (c a)))) c))

(def lift-env (lift-cont-t (env-t error)))

(defn lift-error [mv] (lift-env (lift-env-t mv)))

; interpreter

(def interp-monad (cont-t (env-t error)))

(defn report-error [desc] (lift-error (fail desc)))

(defn add-to-env [e k v] (assoc e k v))

(defn interp-lookup [k] (lift-env (ask-env-t error k)))

(def interp-capture-env (lift-env (capture-env-t error)))

(defn interp-local-env [f mv]
   (fn [c]
     (domonad (env-t error)
        [e (capture-env-t error)
         r (local-env f (mv (fn [x] (local-env (fn [_] e) (c x)))))]
         r)))

; closures
(defstruct closure :env :body)

(defn make-closure [env body] (struct closure env body))

(defn closure? [c] (and (map? c) (get c :env) (get c :body)))

; interp is a hack to fake top-level recursion
(defn interp-closure [c interp]
  (interp-local-env (fn [_] (get c :env)) (interp (get c :body))))

(defn interp [e]
  ; (prn "interp: " e)
  (cond
   (symbol? e) (domonad interp-monad
		  [v (interp-lookup e)
		   r (if v
		         (interp v)
			 (report-error (str "undefined variable " e)))]
		  r)
   (number? e) (domonad interp-monad [] e)
   (closure? e) (interp-closure e interp)
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
					    (report-error "division by 0"))]
				     r)
		  (= t 'lambda-v) (domonad interp-monad
				     [ce interp-capture-env]
				     (fn [arg_cl] (domonad interp-monad
						     [arg_val (interp arg_cl)
                                                      new_env (m-result (add-to-env ce (first args) arg_val))
						      body_cl (m-result (make-closure new_env (second args)))
						      r       (interp body_cl)]
						     r)))
		  (= t 'lambda-n) (domonad interp-monad
				     [ce interp-capture-env]
				     (fn [arg_cl]
                                       (let [new-env (add-to-env ce (first args) arg_cl)
                                             body_cl (make-closure new-env (second args))]
                                            (interp body_cl))))
		  :default (domonad interp-monad
                             [f (interp (first e))
                              ce interp-capture-env
                              r (f (make-closure ce (second e)))]
                             r)
		  ))
   ))

; examples

(def initial-env {'x 7, 'y 21})

; the interpreter continuation needs to handle the underlying monads
(def interp-cont (with-monad (env-t error) m-result))

(defn run-interp [exp]
      (prn 'run-interp exp)
      (prn (run-with-env initial-env ((interp exp) interp-cont))))

;fail: undefined variable: z
(run-interp 'z)

; success: 3
(run-interp 3)

; success: 7
(run-interp 'x)

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
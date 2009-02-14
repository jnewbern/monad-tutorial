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

(defn add-to-env [e k v]
  (with-monad error (assoc e k (m-result v))))

(defn interp [e]
  ; (prn "interp: " e)
  (cond
   (symbol? e) (domonad interp-monad
		  [v (ask-env-t error e)
		   r (if v
		         (m-bind (with-monad env (m-result v)) interp)
			 (report-error (str "undefined variable " e)))]
		  r)
   (number? e) (domonad interp-monad [] e)
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
		  (= t 'closure)  (domonad interp-monad
				     [r (local-env (fn [_] (first args)) (interp (second args)))]
				     r)
		  (= t 'lambda-v) (domonad interp-monad
				     [ce (capture-env-t error)]
				     (fn [arg_cl] (domonad interp-monad
						     [arg_val (interp arg_cl)
						      body_cl (m-result (list 'closure (add-to-env ce (first args) arg_val) (second args)))
						      r       (interp body_cl)]
						     r)))
		  (= t 'lambda-n) (domonad interp-monad
				     [ce (capture-env-t error)]
				     (fn [arg_cl] (domonad interp-monad
						     [body_cl (m-result (list 'closure (add-to-env ce (first args) arg_cl) (second args)))
						      r       (interp body_cl)]
						     r)))
		  (= t 'app)      (domonad interp-monad
				     [f  (interp (first args))
				      ce (capture-env-t error)
				      r  (f (list 'closure ce (second args)))]
				     r)
		  ))
   ))

; examples

(def initial-env (with-monad error {'x (m-result 7), 'y (m-result 21)}))

; Success: 17
(prn (run-with-env initial-env
		   (interp '(+ 3 (* 2 x)))))

; Error: undefined variable: z
(prn (run-with-env initial-env
		   (interp '(+ 4 z))))

; Success: 4
(prn (run-with-env initial-env
		   (interp '(/ 12 3))))

; Error: division by 0
(prn (run-with-env initial-env
		   (interp '(/ 12 (- 21 y)))))

; Success: 9
(prn (run-with-env initial-env
		   (interp '(app (lambda-v x (* x x)) 3))))

; Success: 203
(prn (run-with-env initial-env
		   (interp '(+ x (app (lambda-v x (* x x)) (- y x))))))

; Success: 203
(prn (run-with-env initial-env
		   (interp '(+ x (app (lambda-n x (* x x)) (- y x))))))

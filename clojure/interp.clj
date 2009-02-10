(ns monad-tutorial
  (:use clojure.contrib.monads))

; environment monad

(defmonad env
  "Monad which allows a computation to access
   values from an environment"
  [m-result (fn m-result-env [v]
	      (fn [_] v))
   m-bind   (fn m-bind-env [mv f]
	      (fn [e] ((f (mv e)) e)))
   ])

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


(defn capture-env [e] e)

; interpreter

(def interp-monad (env-t maybe))

(defn add-to-map [k v]
  (fn [m] (assoc m k v)))

(defn interp [e]
  (cond
   (symbol? e) (domonad interp-monad
		  [v (ask-env e)
		   r (interp v)]
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
				      :when (not (and (number? y) (= 0 y)))]
				     (/ x y))
		  (= t 'lambda-v) (domonad interp-monad
				     [ce capture-env]
				     (fn [arg] (domonad interp-monad
						  [arg_val (interp arg)]
						  (run-with-env (assoc ce (first args) arg_val)
								(interp (second args))))))
		  (= t 'lambda-n) (domonad interp-monad
				     [ce capture-env]
				     (fn [arg] (domonad interp-monad
						  []
						  (run-with-env (assoc ce (first args) arg)
								(interp (second args))))))
		  (= t 'app)      (domonad interp-monad
				     [f (interp (first args))
				      r (f (second args))]
				     r)
		  ))
   ))

; examples

(def initial-env {'x 7, 'y 21})

; Success: 17
(prn (run-with-env initial-env
		   (interp '(+ 3 (* 2 x)))))

; Error: undefined variable "z"
(prn (run-with-env initial-env
		   (interp '(+ 4 z))))

; Success: 4
(prn (run-with-env initial-env
		   (interp '(/ 12 3))))

; Error: division by 0
(prn (run-with-env initial-env
		   (interp '(/ 12 (- 21 y)))))

; Success: 203
(prn (run-with-env initial-env
		   (interp '(+ x (app (lambda-v x (* x x)) (- y x))))))

; Success: 203
(prn (run-with-env initial-env
		   (interp '(+ x (app (lambda-n x (* x x)) (- y x))))))

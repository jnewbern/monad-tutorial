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
		  (= t 'closure)  (domonad interp-monad
				     []
				     (run-with-env (first args) (interp (second args))))
		  (= t 'lambda-v) (domonad interp-monad
				     [ce capture-env]
				     (fn [arg_cl] (domonad interp-monad
						     [arg_val (interp arg_cl)
						      body_cl (m-result (list 'closure (assoc ce (first args) arg_val) (second args)))
						      r       (interp body_cl)]
						     r)))
		  (= t 'lambda-n) (domonad interp-monad
				     [ce capture-env]
				     (fn [arg_cl] (domonad interp-monad
						     [body_cl (m-result (list 'closure (assoc ce (first args) arg_cl) (second args)))
						      r       (interp body_cl)]
						     r)))
		  (= t 'app)      (domonad interp-monad
				     [f  (interp (first args))
				      ce capture-env
				      r  (f (list 'closure ce (second args)))]
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

; Success: 9
(prn (run-with-env initial-env
		   (interp '(app (lambda-v x (* x x)) 3))))

; Success: 203
(prn (run-with-env initial-env
		   (interp '(+ x (app (lambda-v x (* x x)) (- y x))))))

; Success: 203
(prn (run-with-env initial-env
		   (interp '(+ x (app (lambda-n x (* x x)) (- y x))))))

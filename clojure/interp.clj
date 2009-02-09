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

; arithmetic language fragment

(defstruct expr :tag :val)

(defn literal [n] (struct expr 'literal n))
(defn plus [x y] (struct expr 'plus [x y]))
(defn minus [x y] (struct expr 'minus [x y]))
(defn times [x y] (struct expr 'times [x y]))
(defn divide [x y] (struct expr 'divide [x y]))

; variable references

(defn variable [ident] (struct expr 'var ident))

; interpreter

(def interp-monad (env-t maybe))

(defn interp [e]
  (let [t (:tag e)]
    (cond
       (= t 'literal) (domonad interp-monad
			 []
			 (:val e))
       (= t 'plus)    (domonad interp-monad
			 [x (interp (first (:val e)))
			  y (interp (second (:val e)))]
			 (+ x y))
       (= t 'minus)   (domonad interp-monad
			 [x (interp (first (:val e)))
			  y (interp (second (:val e)))]
			 (- x y))
       (= t 'times)   (domonad interp-monad
			 [x (interp (first (:val e)))
			  y (interp (second (:val e)))]
			 (* x y))
       (= t 'divide)  (domonad interp-monad
			 [x (interp (first (:val e)))
			  y (interp (second (:val e)))
			  :when (not (= y 0))]
			 (/ x y))
       (= t 'var)     (domonad interp-monad
			 [v (ask-env (:val e))
			  r (interp v)]
                         r)
       )))

; examples

(def initial-env {"x" (literal 7), "y" (literal 21)})

(prn (run-with-env initial-env
		   (interp (plus (literal 3) (times (literal 2) (variable "x"))))))

(prn (run-with-env initial-env
		   (interp (plus (literal 4) (variable "z")))))

(prn (run-with-env initial-env
		   (interp (divide (literal 12) (literal 3)))))

(prn (run-with-env initial-env
		   (interp (divide (literal 12) (minus (literal 21) (variable "y"))))))

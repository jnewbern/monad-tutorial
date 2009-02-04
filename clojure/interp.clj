(ns monad-tutorial
  (:use clojure.contrib.monads))

; arithmetic language fragment

(defstruct expr :tag :val)

(defn literal [n] (struct expr 'literal n))
(defn plus [x y] (struct expr 'plus [x y]))
(defn minus [x y] (struct expr 'minus [x y]))
(defn times [x y] (struct expr 'times [x y]))
(defn divide [x y] (struct expr 'divide [x y]))

(defn interp [e]
  (let [t (:tag e)]
    (cond
       (= t 'literal) (domonad maybe
			 []
			 (:val e))
       (= t 'plus)    (domonad maybe
			 [x (interp (first (:val e)))
			  y (interp (second (:val e)))]
			 (+ x y))
       (= t 'minus)   (domonad maybe
			 [x (interp (first (:val e)))
			  y (interp (second (:val e)))]
			 (- x y))
       (= t 'times)   (domonad maybe
			 [x (interp (first (:val e)))
			  y (interp (second (:val e)))]
			 (* x y))
       (= t 'divide)  (domonad maybe
			 [x (interp (first (:val e)))
			  y (interp (second (:val e)))
			  :when (not (= y 0))]
			 (/ x y)))))


; examples

; (prn (interp (plus (literal 3) (times (literal 2) (literal 7)))))

; (prn (interp (divide (literal 12) (literal 3))))

; (prn (interp (divide (literal 12) (minus (literal 3) (literal 3)))))


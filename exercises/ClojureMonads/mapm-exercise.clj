(ns monad-tutorial
  (:use newmonads))

; ----------------------------------------------------------------------
; Exercise: mapm
;
; The mapm function is used to map a monadic function over a list of
; values.  It returns its list of results in the monad.  It's type
; would be (a -> b-in-m) -> list-of-a -> (list-of-b)-in-m
;
; For this exercise, complete the partial definition of the mapm
; function given below by replacing each occurence of ??? with
; some Clojure code.
; ----------------------------------------------------------------------


; Function: mapm
; Args:
;  mf - a monadic function (takes one argument and returns a value
;       in the monad)
;  l  - a list of values suitable as arguments to mf
; Result: A list of results in the monad

(defmonadfn mapm [f xs]
  (let [ ; map f to create a list of monadic values
         mvs (???)
	 ; flipped cons lifted into the monad
	 m-flipped-cons (??? (fn [lst el] (cons el lst)))
       ]
    ???))


; ----------------------------------------------------------------------
; An example use of mapm
;
; In this example we have a state structure which records
; the number of even and odd values seen.
;
; We have a "div2" function that does an integer division by 2
; and records whether the input was even or odd.

(defstruct counts :odds :evens)

(defn incr-evens [c] (struct counts (get c :odds) (+ 1 (get c :evens))))
(defn incr-odds  [c] (struct counts (+ 1 (get c :odds)) (get c :evens)))

; divide by 2 and increment even or odd counter
(defn div2 [x]
  (domonad state-m
	   [s    (m-get)
	    news (m-result (if (even? x) (incr-evens s) (incr-odds s)))
	    _    (m-put news)
	   ]
	   (if (even? x) (/ x 2) (/ (- x 1) 2))))

; this should print ((5 3 1 7 14 1 25 16) {:odds 3, :evens 5})
(let [initial-state (struct counts 0 0)
      div-list      (domonad state-m
			     [ns (mapm div2 (list 10 7 3 14 28 2 50 33))]
			     ns)
     ]
  (prn (div-list initial-state)))

(ns monad-tutorial)

(load-file "interp.clj")

; a language fragment that accumulates a log of numbers in a string
; we also provide the ability to print them during execution

; our log is just a string
(def empty-log clojure.contrib.accumulators/empty-string)

; required utility
(defn write-number [m v]
  (with-monad m 
    ; write a number followed by a space to the log
    ; if not a number fail with an error
    nil))


(def log-lang
  [ ; log the values of all the arguments
    ; returns nil
   [ (fn [e]
       (and (seq? e) (= (first e) 'write-nums)))
     (fn [m rec e]
       (domonad m
	 [args (m-result (rest e))
	  ; fill in body
	  ]
	 nil)) ]
   ,
   ; collect and print the numbers logged while evaluating
   ; the list of argument expressions
   ; return the last expression as the result
   [ (fn [e] (and (seq? e) (= (first e) 'collect-nums)))
     (fn [m rec e]
       (domonad m
         [args (m-result (rest e))
	  ; fill in the correct right-hand-size of this binding
	  [vals log] [nil "broken log"]
	  _ (m-result (println (str "number log: " log)))
	  ]
	 (last vals))) ]
   ,
   ; censor any values logged while evaluating its argument
   [ (fn [e] (and (seq? e) (= (first e) 'censor-nums)))
     (fn [m rec e]
       (with-monad m
	 ; correct the body
	 ; m-result nil is a placeholder
	 ; keep the value returned the same while censoring
	 (m-result nil)))
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
    (test "log-fail" '(write-nums (write-nums 5)) '(fail "write non-number: "))
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

; if you've finished this you should be able to experiment with
; assembling your own interpreters and seeing how they behave

; one particularly interesting thing to observe is how the 
; behavior of other effects in relation to call-cc changes
; as you re-order them with respect to the cont-t transformer

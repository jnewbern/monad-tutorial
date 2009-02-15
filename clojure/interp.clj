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

(defn callcc [f] (fn [c] ((f (fn [a] (fn [_] (c a)))) c)))

; eval-cont-t unwraps a continuation value by providing
; a default continuation (the lower-level m-result)
(defn eval-cont-t [m mv] (with-monad m (mv m-result)))

; state monad transformer
(defn state-t
   "Monad transformer that adds local state to an existing monad"
   [m]
   (monad [m-result (fn m-result-state-t [v]
                       (with-monad m (fn [s] (m-result (list v s)))))
           m-bind   (fn m-bind-state-t [mv f]
                       (fn [s]
                          (domonad m
                            [[v ss] (mv s)
                              r ((f v) ss)]
                            r)))]))

(defn lift-state-t [m]
   (fn [mv]
     (fn [s]
        (domonad m [v mv] (list v s)))))

(defn get-state-t [m]
    (fn [s] (domonad m [] (list s s))))

(defn put-state-t [m ss]
    (fn [s] (domonad m [] (list nil ss))))

(defn eval-state-t [m]
   (fn [mv s] (domonad m [v (mv s)]
                         (do (prn "final state: " (second v))
                             (first v)))))

; interpreter

(def interp-monad (state-t (cont-t (env-t error))))

(def lift-cont (lift-state-t (cont-t (env-t error))))

(def lift-env (comp lift-cont (lift-cont-t (env-t error))))

(def lift-error (comp lift-env lift-env-t))

(defn report-error [desc] (lift-error (fail desc)))

(defn add-to-env [e k v] (assoc e k v))

(defn interp-lookup [k] (lift-env (ask-env-t error k)))

(def interp-capture-env (lift-env (capture-env-t error)))

; rebuild local-env around a continuation monad transformer
(defn cont-local-env [m local-env-m capture-env-m]
   (fn [f mv]
     (fn [c]
       (domonad m
          [e capture-env-m
           r (local-env-m f (mv (fn [x] (local-env-m (fn [_] e) (c x)))))]
           r))))

; lift callcc over a state monad

(defn callcc-state-t [callcc-m]
   (fn [f] ; function expecting a continuation
     (fn [s] (callcc-m (fn [c] ((f (fn [v] (fn [_] (c (list v s))))) s))))))

(defn local-env-state-t [local-env-m]
  (fn [f mv]
    (fn [s] (local-env-m f (mv s)))))

(def interp-local-env
   (local-env-state-t (cont-local-env (env-t error) local-env (capture-env-t error))))

(def interp-callcc (callcc-state-t callcc))

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
                  (= t 'callcc) (domonad interp-monad
                                  [ce interp-capture-env
                                   r (interp-callcc
                                       (fn [cont]
                                         (let [new-cont
                                                (fn [arg]
						  (domonad interp-monad
                                                      [v (interp arg)
                                                       r (cont v)]
                                                      r))
                                               new-env (add-to-env ce (first args) new-cont)
                                               body_cl (make-closure new-env (second args))]
                                              (interp body_cl))))]
                                   r)
                  (= t 'do) (if (empty? args)
                                (report-error "nothing to do")
                                (let [to_do (map interp args)]
                                     (domonad interp-monad
                                        [results (m-seq to_do)]
                                        (last results))))
		  :default (domonad interp-monad
                             [f (interp (first e))
                              ce interp-capture-env
                              r (f (make-closure ce (second e)))]
                             r)
		  ))
   ))

; examples

(def initial-env {'x 7, 'y 21})

(def initial-state "initial state")

(def run-interp-state (eval-state-t (cont-t (env-t error))))

(defn run-interp [exp]
      (prn 'run-interp exp)
      (prn (run-with-env initial-env
             (eval-cont-t (env-t error)
               (run-interp-state (interp exp) initial-state)))))

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
(run-interp '(callcc exit (+ 5 (exit 3))))

; success 14
(run-interp '(+ (callcc exit (+ 7 (exit 9)))
                (+ 2 (callcc exit (- 2 (exit 3))))))

; success 25
(run-interp '(* (callcc exit (/ (exit 5) 0))
               5))

; success 10
(run-interp '((callcc exit-fn (lambda-v n (* n 2))) 5))

; continuation escaping callcc
; success 30
(run-interp '((callcc exit-fn
                (lambda-v n
                   (+ n
                     (exit-fn (lambda-v r (* n (+ r 1))))))) 5))



; fail - nothing to do
(run-interp '(do))

; success 7
(run-interp '(do 5 7))

(ns monad-tutorial
  (:use newmonads))

(defn trace [s x] (do (prn s x) x))

; interpreter

(def interp-monad (state-t (cont-t (env-t error-m))))

(def lift-cont (lift-state-t (cont-t (env-t error-m))))

(def lift-env (comp lift-cont (lift-cont-t (env-t error-m))))

(def lift-error (comp lift-env lift-env-t))

(defn report-error [desc] (lift-error (fail desc)))

(defn add-to-env [e k v] (assoc e k v))

(defn interp-lookup [k] (lift-env (ask-env-t error-m k)))

(def interp-capture-env (lift-env (capture-env-t error-m)))

(def interp-get-state (get-state-t (cont-t (env-t error-m))))

(def interp-put-state (put-state-t (cont-t (env-t error-m))))

(defstruct interp-state :cells)

; rebuild local-env around a continuation monad transformer
(defn cont-local-env [m local-env-m capture-env-m]
   (fn [f mv]
     (fn [c]
       (domonad m
          [e capture-env-m
           r (local-env-m f (mv (fn [x] (local-env-m (fn [_] e) (c x)))))]
           r))))

; lift call-cc over a state monad
; restores state when calling continuation
(defn call-cc-state-t [call-cc-m]
   (fn [f] ; function expecting a continuation
     (fn [s] (call-cc-m (fn [c] ((f (fn [v] (fn [_] (c (list v s))))) s))))))

; state-preserving version of call-cc
; equivalent to (cont-t (state-t)) instead of (state-t (cont-t))
(defn alt-call-cc-state-t [call-cc-m]
   (fn [f] ; function expecting a continuation
     (fn [s] (call-cc-m (fn [c] ((f (fn [v] (fn [ss] (c (list v ss))))) s))))))

(defn local-env-state-t [local-env-m]
  (fn [f mv]
    (fn [s] (local-env-m f (mv s)))))

(def interp-local-env
   (local-env-state-t (cont-local-env (env-t error-m) local-env (capture-env-t error-m))))

(def interp-call-cc (call-cc-state-t call-cc))

(def interp-alt-call-cc (alt-call-cc-state-t call-cc))

; closures
(defstruct closure :env :body)

(defn make-closure [env body] (struct closure env body))

(defn closure? [c] (and (map? c) (get c :env) (get c :body)))

; interp is a hack to fake top-level recursion
(defn interp-closure [c interp]
  (interp-local-env (fn [_] (get c :env)) (interp (get c :body))))

; wrap continuation into usable function
; interp hack again
(defn interp-wrap-cont [interp cont]
  (fn [arg] (domonad interp-monad
              [v (interp arg)
               r (cont v)]
              r)))

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
		  (= t 'new-ref)  (domonad interp-monad
				     [val  (interp (first args))
				      s    interp-get-state
				      refv (m-result (get s :cells))
				      idx  (m-result (count refv))
				      newv (m-result (conj refv val))
				      news (m-result (assoc s :cells newv))
				      _    (interp-put-state news)]
				     idx)
		  (= t 'read)     (domonad interp-monad
				     [idx  (interp (first args))
				      s    interp-get-state
				      refv (m-result (get s :cells))
				      v    (m-result (nth refv idx 'not-found))
				      x    (if (= v 'not-found)
					     (report-error "invalid reference")
					     (interp v))]
				     x)
		  (= t 'write)    (domonad interp-monad
				     [idx  (interp (first args))
				      val  (interp (second args))
				      s    interp-get-state
				      refv (m-result (get s :cells))
				      newv (m-result (assoc refv idx val))
				      news (m-result (assoc s :cells newv))
				      _    (interp-put-state news)]
				     nil)
                  (= t 'call-cc) (domonad interp-monad
                                  [ce interp-capture-env
                                   r (interp-call-cc
                                       (fn [cont]
                                         (let [new-cont (interp-wrap-cont interp cont)
                                               new-env (add-to-env ce (first args) new-cont)
                                               body_cl (make-closure new-env (second args))]
                                              (interp body_cl))))]
                                   r)
                  (= t 'alt-call-cc) (domonad interp-monad
                                      [ce interp-capture-env
                                       r (interp-alt-call-cc
                                           (fn [cont]
                                             (let [new-cont (interp-wrap-cont interp cont)
                                                   new-env  (add-to-env ce (first args) new-cont)
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

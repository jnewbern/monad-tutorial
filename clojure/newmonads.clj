;; Monads in Clojure

;; by Konrad Hinsen
;; last updated March 3, 2009

;; Copyright (c) Konrad Hinsen, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns newmonads
  (:require [clojure.contrib.accumulators]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Defining monads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro monad
   "Define a monad by defining the monad operations. The definitions
    are written like bindings to the monad operations m-bind and
    m-result (required) and m-zero and m-plus (optional)."
   [operations]
   `(let [~'m-bind   ::undefined
	  ~'m-result ::undefined
	  ~'m-base   ::undefined
	  ~'t-base   ::undefined
	  ~'t-map    ::undefined
	  ~'m-zero   ::undefined
	  ~'m-plus   ::undefined
	  ~'m-get    ::undefined
	  ~'m-put    ::undefined
	  ~'m-capture-env ::undefined
	  ~'m-local-env ::undefined
	  ~'m-call-cc ::undefined
	  ~'m-fail   ::undefined
	  ~@operations]
      {:m-result ~'m-result
       :m-bind ~'m-bind 
       :m-base ~'m-base
       :t-base ~'t-base
       :t-map  ~'t-map
       :m-zero ~'m-zero
       :m-plus ~'m-plus
       :m-get  ~'m-get
       :m-put  ~'m-put
       :m-capture-env ~'m-capture-env
       :m-local-env ~'m-local-env
       :m-call-cc ~'m-call-cc
       :m-fail ~'m-fail}))

(defmacro when-defined [m op new-val]
  `(if (= (with-monad ~m ~op) ::undefined)
      ::undefined
      ~new-val))
 
(defmacro defmonad
   "Define a named monad by defining the monad operations. The definitions
    are written like bindings to the monad operations m-bind and
    m-result (required) and m-zero and m-plus (optional)."

   ([name doc-string operations]
    (let [doc-name (with-meta name {:doc doc-string})]
      `(defmonad ~doc-name ~operations)))

   ([name operations]
    `(def ~name (monad ~operations))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Using monads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- add-monad-step
  "Add a monad comprehension step before the already transformed
   monad comprehension expression mexpr."
  [mexpr step]
  (let [[bform expr] step]
    (if (identical? bform :when)
      `(if ~expr ~mexpr ~'m-zero)
      (list 'm-bind expr (list 'fn [bform] mexpr)))))

(defn- monad-expr
   "Transforms a monad comprehension, consisting of a list of steps
    and an expression defining the final value, into an expression
    chaining together the steps using :bind and returning the final value
    using :result. The steps are given as a vector of
    binding-variable/monadic-expression pairs."
   [steps expr]
   (when (odd? (count steps))
     (throw (Exception. "Odd number of elements in monad comprehension steps")))
   (let [rsteps (reverse (partition 2 steps))
	 [lr ls] (first rsteps)]
     (if (= lr expr)
       ; Optimization: if the result expression is equal to the result
       ; of the last computation step, we can eliminate an m-bind to
       ; m-result.
       (reduce add-monad-step
	       ls
	       (rest rsteps))
       ; The general case.
       (reduce add-monad-step
	       (list 'm-result expr)
	       rsteps))))

(defmacro with-monad
   "Evaluates an expression after replacing the keywords defining the
    monad operations by the functions associated with these keywords
    in the monad definition given by name."
   [name & exprs]
   `(let [~'m-bind   (:m-bind ~name)
	  ~'m-result (:m-result ~name)
	  ~'m-base   (:m-base ~name)
	  ~'t-base   (:t-base ~name)
	  ~'t-map    (:t-map ~name)
	  ~'m-zero   (:m-zero ~name)
	  ~'m-plus   (:m-plus ~name)
	  ~'m-get    (:m-get ~name)
	  ~'m-put    (:m-put ~name)
	  ~'m-capture-env (:m-capture-env ~name)
	  ~'m-local-env (:m-local-env ~name)
	  ~'m-call-cc (:m-call-cc ~name)
	  ~'m-fail   (:m-fail ~name)]
      (do ~@exprs)))

(defmacro with-base-monad
   "Evaluates an expression within the interior monad of a 
    transformed monad and then lifts the result to the 
    combined monad"
    [name & exprs]
    `(with-monad ~name
       (~'t-base (with-monad ~'m-base ~@exprs))))
 
(defmacro domonad
   "Monad comprehension. Takes the name of a monad, a vector of steps
    given as binding-form/monadic-expression pairs, and a result value
    specified by expr. The monadic-expression terms can use the binding
    variables of the previous steps. If the monad contains a definition
    of :zero, the step list can also contain conditions of the form [:when p],
    where the predicate p can contain the binding variables from all previous
    steps."
   ([steps expr]
    (monad-expr steps expr))
   ([name steps expr]
    (let [mexpr (monad-expr steps expr)]
      `(with-monad ~name ~mexpr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Defining functions used with monads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defmonadfn
  "Like defn, but for functions that use monad operations and are used inside
   a with-monad block."

  ([name doc-string args expr]
    (let [doc-name (with-meta name {:doc doc-string})]
      `(defmonadfn ~doc-name ~args ~expr)))

  ([name args expr]
   (let [fn-name (symbol (str *ns*) (format "m+%s+m" (str name)))]
   `(do
      (defmacro ~name ~args
        (list (quote ~fn-name)
	      '~'m-bind '~'m-result
              '~'m-base '~'t-base '~'t-map
	      '~'m-get  '~'m-put
	      '~'m-capture-env '~'m-local-env
	      '~'m-call-cc
	      '~'m-zero '~'m-plus
	      '~'m-fail
	      ~@args))
      (defn ~fn-name [~'m-bind ~'m-result
		      ~'m-base ~'t-base ~'t-map
		      ~'m-get  ~'m-put
		      ~'m-capture-env ~'m-local-env
		      ~'m-call-cc
		      ~'m-zero ~'m-plus
		      ~'m-fail
		      ~@args] ~expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commonly used monad functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro m-lift
  "Converts a function f of n arguments into a function of n
  monadic arguments returning a monadic value."
  [n f]
  (let [expr (take n (repeatedly #(gensym "x_")))
	vars (vec (take n (repeatedly #(gensym "mv_"))))
	steps (vec (interleave expr vars))]
    (list `fn vars (monad-expr steps (cons f expr)))))

(defmonadfn m-join
  "Converts a monadic value containing a monadic value into a 'simple'
   monadic value."
  [m]
  (m-bind m identity))

(defmonadfn m-fmap
  "Bind the monadic value m to the function returning (f x) for argument x"
  [f m]
  (m-bind m (fn [x] (m-result (f x)))))

(defmonadfn m-seq
  "'Executes' the monadic values in ms and returns a sequence of the
   basic values contained in them."
  [ms]
  (reduce (fn [q p]
	    (m-bind p (fn [x]
			(m-bind q (fn [y]
				    (m-result (cons x y)))) )))
	  (m-result '())
	  (reverse ms)))

(defmonadfn m-map
  "'Executes' the sequence of monadic values resulting from mapping
   f onto the values xs. f must return a monadic value."
  [f xs]
  (m-seq (map f xs)))

(defmonadfn m-chain
  "Chains together monadic computation steps that are each functions
   of one parameter. Each step is called with the result of the previous
   step as its argument. (m-chain (step1 step2)) is equivalent to
   (fn [x] (domonad [r1 (step1 x) r2 (step2 r1)] r2))."
  [steps]
  (reduce (fn m-chain-link [chain-expr step]
	    (fn [v] (m-bind (chain-expr v) step)))
	  m-result
	  steps))

(defmacro m-when
  "If test if logical true, return monadic value m-expr, else return
   (m-result nil)."
  [test m-expr]
  `(if ~test ~m-expr (~'m-result nil)))

(defmacro m-when-not
  "If test if logical false, return monadic value m-expr, else return
   (m-result nil)."
  [test m-expr]
  `(if ~test (~'m-result nil) ~m-expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commonly used monads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Identity monad
(defmonad identity-m
   "Monad describing plain computations. This monad does in fact nothing
    at all. It is useful for testing, for combination with monad
    transformers, and for code that is parameterized with a monad."
  [m-result identity
   m-bind   (fn m-result-id [mv f]
	      (f mv))
  ])

; Maybe monad
(defmonad maybe-m
   "Monad describing computations with possible failures. Failure is
    represented by nil, any other value is considered valid. As soon as
    a step returns nil, the whole computation will yield nil as well."
   [m-zero   nil
    m-result (fn m-result-maybe [v] v)
    m-bind   (fn m-bind-maybe [mv f]
               (if (nil? mv) nil (f mv)))
    m-plus   (fn m-plus-maybe [& mvs]
	       (first (drop-while nil? mvs)))
    ])

; Sequence monad (called "list monad" in Haskell)
(defmonad sequence-m
   "Monad describing multi-valued computations, i.e. computations
    that can yield multiple values. Any object implementing the seq
    protocol can be used as a monadic value."
   [m-result (fn m-result-sequence [v]
	       (list v))
    m-bind   (fn m-bind-sequence [mv f]
               (apply concat (map f mv)))
    m-zero   (list)
    m-plus   (fn m-plus-sequence [& mvs]
               (apply concat mvs))
    ])

; State monad
(defn update-state [f]
  (fn [s] (list s (f s))))

(defn set-state [s]
  (update-state (fn [_] s)))

(defn fetch-state []
  (update-state identity))

(defmonad state-m
   "Monad describing stateful computations. The monadic values have the
    structure (fn [old-state] (list result new-state))."
   [m-result  (fn m-result-state [v]
	        (fn [s] (list v s)))
    m-bind    (fn m-bind-state [mv f]
	        (fn [s]
		  (let [[v ss] (mv s)]
		    ((f v) ss))))
    m-get     fetch-state
    m-put     set-state
   ])

; Writer monad
(defn writer-m
  "Monad describing computations that accumulate data on the side, e.g. for
   logging. The monadic values have the structure [value log]. Any of the
   accumulators from clojure.contrib.accumulators can be used for storing the
   log data. Its empty value is passed as a parameter."
  [empty-accumulator]
  (monad
     [m-result  (fn m-result-writer [v]
	          [v empty-accumulator])
      m-bind    (fn m-bind-writer [mv f]
	          (let [[v1 a1] mv
			[v2 a2] (f v1)]
		    [v2 (clojure.contrib.accumulators/combine a1 a2)]))
     ]))

(defmonadfn write [v]
  (let [[_ a] (m-result nil)]
    [nil (clojure.contrib.accumulators/add a v)]))

(defn listen [mv]
  (let [[v a] mv] [[v a] a]))

(defn censor [f mv]
  (let [[v a] mv] [v (f a)]))

; Continuation monad

(defmonad cont-m
  "Monad describing computations in continuation-passing style. The monadic
   values are functions that are called with a single argument representing
   the continuation of the computation, to which they pass their result."
  [m-result   (fn m-result-cont [v]
		(fn [c] (c v)))
   m-bind     (fn m-bind-cont [mv f]
		(fn [c]
		  (mv (fn [v] ((f v) c)))))
   ])

(defn run-cont
  "Execute the computation c in the cont monad and return its result."
  [c]
  (c identity))

(defn call-cc
  "A computation in the cont monad that calls function f with a single
   argument representing the current continuation. The function f should
   return a continuation (which becomes the return value of call-cc),
   or call the passed-in current continuation to terminate."
  [f]
  (fn [c]
    (let [cc (fn cc [a] (fn [_] (c a)))
	  rc (f cc)]
      (rc c))))

(defn fail [err] (list 'fail err))

; error monad
(defmonad error-m
  "Monad describing computations with possible failures.
   Values in the monad pair a status code with either a value
   or a failure descriptor."
  [m-result  (fn m-result-error [v]  (list 'ok v))
   m-bind    (fn m-bind-error [mv f] (if (= 'ok (first mv))
				         (f (second mv))
					 mv))
   m-fail    fail
   ])

(defn success? [mv] (= 'ok (first mv)))
(defn failure? [mv] (not (success? mv)))

(defn successful-value [mv] (if (success? mv) (second mv) 'nil))
(defn error-desc       [mv] (if (failure? mv) (second mv) 'nil))

; environment monad
(defn local-env [f mv]
  (fn [e] (mv (f e))))

(defmonad env-m
  "Monad which allows a computation to access
   values from an environment"
  [m-result (fn m-result-env [v]
	      (fn [_] v))
   m-bind   (fn m-bind-env [mv f]
	      (fn [e] ((f (mv e)) e)))
   m-capture-env 
            (fn m-capture-env [e] e)
   m-local-env local-env 
   ])

(defn run-with-env [e mv]
  (mv e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Monad transformers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn maybe-t
  "Monad transformer that transforms a monad m into a monad in which
   the base values can be invalid (represented by nothing, which defaults
   to nil). The third argument chooses if m-zero and m-plus are inherited
   from the base monad (use :m-plus-from-base) or adopt maybe-like
   behaviour (use :m-plus-from-maybe)."
  ([m] (maybe-t m nil :m-plus-from-base))
  ([m nothing which-m-plus]
   (let [combined-m-zero
	 (cond
	  (identical? which-m-plus :m-plus-from-base)
	  (with-monad m m-zero)
	  (identical? which-m-plus :m-plus-from-maybe)
	  (with-monad m (m-result nothing))
	  :else ::undefined)
	 combined-m-plus
	 (cond
	  (identical? which-m-plus :m-plus-from-base)
	  (with-monad m m-plus)
	  (identical? which-m-plus :m-plus-from-maybe)
	  (with-monad m
	    (fn [& mvs]
	      (m-result (loop [mv (first mvs)]
			  (if (nil? mv)
			    nothing
			    (let [v (m-bind mv identity)]
			      (if (identical? v nothing)
				(recur (rest mvs))
				v)))))))
	  :else ::undefined)]
     (monad [m-result (with-monad m
		        m-result)
	     m-bind   (with-monad m
		        (fn m-bind-maybe-t [mv f]
			  (m-bind mv
				  (fn [x]
				    (if (identical? x nothing)
				      (m-result nothing)
				      (f x))))))
	     m-zero   combined-m-zero
	     m-plus   combined-m-plus
	     ]))))

(defn sequence-t
  "Monad transformer that transforms a monad m into a monad in which
   the base values are sequences."
  [m]
  (monad [m-result (with-monad m
		     (fn m-result-sequence-t [v]
		       (m-result (list v))))
	  m-bind   (with-monad m
		     (fn m-bind-sequence-t [mv f]
		       (m-bind mv
			       (fn [xs]
				 (apply concat (map f xs))))))
	  m-base   m
	  t-base   (fn t-base-sequence-t [mv]
		     (with-monad m
		       (m-bind mv (fn [v] (m-result (list v))))))
	  t-map    (fn t-map-sequence-t [mop]
		     (fn [mv]
		       (with-monad m
			 (m-bind mv
			   (fn [xs]
			     (m-seq (map (comp mop m-result) xs)))))))
	  m-fail   (fn [desc] (domonad m [] (list)))
	  ]))

; state monad transformer
; based on a transformer written by Jim Duey
(defn lift-state-t [m]
   (fn [mv]
     (fn [s]
        (domonad m [v mv] (list v s)))))

; lift-call-cc over an arbitrary monad
(defn lift-call-cc [t-m-result t-m-bind t-m-base t-t-base t-t-map]
  (let [join (fn [mma] (t-m-bind mma identity))
        q    (fn [c]   (fn [a] (t-t-base (c (t-m-result a)))))]
    (fn [f]
      (join
       (t-t-base
	(with-monad t-m-base
	  (m-call-cc
	   (fn [c] (m-result (f (q c)))))))))))

; lift local-env over an arbitrary monad
(defn lift-local-env [t-m-result t-m-bind t-m-base t-t-base t-t-map]
  (fn [f mv]
    (with-monad t-m-base
      ((t-t-map (partial m-local-env f)) mv))))

; lift call-cc over a state monad
; restores state when calling continuation
;(defn call-cc-state-t [call-cc-m]
;   (fn [f] ; function expecting a continuation
;     (fn [s]
;       (call-cc-m
;	(fn [c]
;	  ((f (fn [v]
;		(fn [_]
;		  (c (list v s)))))
;	   s))))))

(defn state-t
  "Monad transformer that transforms a monad m into a monad of stateful
  computations that have the base monad type as their result."
  [m]
  (monad [m-result (with-monad m
		     (fn m-result-state-t [v]
                       (fn [s]
			 (m-result (list v s)))))
	  m-bind   (with-monad m
                     (fn m-bind-state-t [stm f]
                       (fn [s]
                         (m-bind (stm s)
                                 (fn [[v ss]]
                                   ((f v) ss))))))
	  m-base   m
	  t-base   (lift-state-t m)
	  t-map    (fn t-map-state-t [mop]
		     (fn [mv] (fn [s] (mop (mv s)))))
	  m-get    (fn [s] (domonad m [] (list s s)))
	  m-put    (fn [ss] (fn [s] (domonad m [] (list nil ss))))
	  m-fail   (when-defined m m-fail
		     (fn [desc]
		       (t-base (with-monad m (m-fail desc)))))
	  m-capture-env
                   (when-defined m m-capture-env
		       (t-base (with-monad m m-capture-env)))
	  m-local-env
                   (when-defined m m-local-env
		     (lift-local-env m-result m-bind m-base t-base t-map))
	  m-call-cc
	           (when-defined m m-call-cc
		     (lift-call-cc m-result m-bind m-base t-base t-map))
          m-zero   (when-defined m m-zero
                     (t-base (with-monad m m-zero)))
          m-plus   (when-defined m m-plus
                     (with-monad m
		       (fn [& stms]
			 (fn [s]
			   (apply m-plus (map #(% s) stms))))))
          ]))

(defn eval-state-t [m]
   (fn [mv s] (domonad m [v (mv s)] (first v))))

; environment monad transformer
(defn env-t
  "Monad transformer that adds an environment to an existing monad"
  [m]
  (monad [m-result (with-monad m
		     (fn m-result-env-t [v]
		       (fn [_] (m-result v))))
	  m-bind   (with-monad m
		     (fn m-bind-env-t [mv f]
		       (fn [e] (m-bind (run-with-env e mv)
				       (fn [x] ((f x) e))))))
	  m-base   m
	  t-base   (fn [mv] (fn [_] mv))
	  t-map    (fn [mop]
		     (fn [mv] (fn [e] (mop (mv e)))))
	  m-capture-env
	           (with-monad m
		      (fn [e] (m-result e)))
	  m-local-env local-env
	  m-get    (when-defined m m-get
                      (t-base (with-monad m m-get)))
	  m-put    (when-defined m m-put
                     (fn [ss]
		       (t-base (with-monad m (m-put ss)))))
	  m-call-cc (when-defined m m-call-cc
		      (lift-call-cc m-result m-bind m-base t-base t-map))
	  m-fail   (when-defined m m-fail
		    (fn [desc]
		      (t-base (with-monad m (m-fail desc)))))
	  ]))

; continuation monad transformer
(defn lift-cont-t [m]
   (fn [mv] (with-monad m (partial m-bind mv))))

; rebuild local-env around a continuation monad transformer
;(defn cont-local-env [m local-env-m capture-env-m]
;   (fn [f mv]
;     (fn [c]
;       (domonad m
;          [e capture-env-m
;           r (local-env-m f (mv (fn [x] (local-env-m (fn [_] e) (c x)))))]
;           r))))

(defn cont-t
  "Monad transformer that adds continuations to an existing monad"
  [m]
  (monad [m-result (fn m-result-cont-t [v]
                      (fn [k] (k v)))
          m-bind   (fn m-bind-cont-t [mv f]
                     (fn [c] (mv (fn [a] ((f a) c)))))
	  m-base   m
	  t-base   (lift-cont-t m)
	  t-map    (fn t-map-cont-t [mop]
		     (fn [mv]
		       (fn [k] (mop (mv (comp mop k))))))
	  m-call-cc call-cc
	  m-get   (when-defined m m-get
		     (t-base (with-monad m m-get)))
	  m-put   (when-defined m m-put
		     (fn [ss]
		       (t-base (with-monad m (m-put ss)))))
	  m-capture-env
	          (when-defined m m-capture-env
		     (t-base (with-monad m m-capture-env)))
	  m-local-env
                  (when-defined m m-local-env
                     (lift-local-env m-result m-bind m-base t-base t-map))
	  m-fail  (when-defined m m-fail
		    (fn [desc]
		      (t-base (with-monad m (m-fail desc)))))
	  ]))

; eval-cont-t unwraps a continuation value by providing
; a default continuation (the lower-level m-result)
(defn eval-cont-t [m mv] (with-monad m (mv m-result)))

; state-preserving version of call-cc
; equivalent to (cont-t (state-t)) instead of (state-t (cont-t))
(defn alt-call-cc-state-t [call-cc-m]
   (fn [f] ; function expecting a continuation
     (fn [s] (call-cc-m (fn [c] ((f (fn [v] (fn [ss] (c (list v ss))))) s))))))

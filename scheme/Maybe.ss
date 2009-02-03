; This module defines a maybe datatype for working with optional values and defines
; monad operations for the maybe datatype.
(module Maybe scheme
  ; exported interface related to maybe data type
  (provide (struct-out maybe) just nothing is-just? is-nothing? from-just)
  ; exported interface for the Maybe monad operations
  (provide return bind)
  
  ; A maybe value is represented as a pair which combines a #t/#f tag with an optional data value.
  (define-struct maybe (valid value))

  ; "just" injects a value into a pair using a tag of #t, indicating that the data is valid
  (define (just x) (make-maybe #t x))

  ; "nothing" uses a tag of #f, indicating that the data portion of the pair is not populated
  (define nothing (make-maybe #f 'nil))

  ; "is-just?" tests the tag to determine if the data is valid
  (define (is-just? v) (maybe-valid v))

  ; "is-nothing?" tests the tag to determine if the data is not populated
  (define (is-nothing? v) (not (maybe-valid v)))

  ; "from-just" extracts the value from a "just" variant of a Maybe value
  (define (from-just v) (maybe-value v))

  
  ; A monad definition for the Maybe data structure
  
  ; returns injects a value into the Maybe monad
  (define (return x) (just x))
  
  ; bind is the monadic sequencing operator on Maybe values.  If the first argument is
  ; a nothing variant, the result is also nothing.  Otherwise, the second argument is
  ; applied to the value extracted from the first -- the second argument should be 
  ; a function of one argument returning a Maybe data structure.
  (define (bind v f)
    (if (is-just? v)
        (f (from-just v))
        nothing))
  
  ;end of module
  )

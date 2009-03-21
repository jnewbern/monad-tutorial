(module ParseString scheme
  (require "List.ss" "Monad.ss")
  (provide parse-string)
  
  ; parse strings into hex values, decimal values or words
  ; possible results are (hex val) (dec val) (word string)
 
  ; Exercise - fill in the missing pieces of:
  ;  - parse-hex-char
  ;  - parse-dec-char
  ;  - parse-word-char
  ;  - parse-char

  ; Test parse-string with the following strings:
  ; "0" "100" "dead" "toast" "f00d"

  (define (hex-digit? char)
    (or (char-numeric? char)
        (member char (string->list "abcdefABCDEF" ))))
  
  (define zero-char-val (char->integer #\0))                                     
  (define A-char-val (char->integer #\A))
  
  (define (hex-digit-to-int c)
    (let ((char-val (char->integer (char-upcase c))))
      (cond ((char-numeric? c) (- char-val zero-char-val))
            ((char-alphabetic? c) (+ 10 (- char-val A-char-val))))))
  
  (define (parse-hex-char parse c)
    (let ((tag (car parse))
          (val (cadr parse)))
      (cond 
        ((and (eq? tag 'hex) (hex-digit? c))
         (let ((new-val (+ (* 16 val) (hex-digit-to-int c))))
	   ; ... fill in ...
	   (return `(hex ,new-val))
	   ))
        (else ; ... fill in ...
	 m-zero
	 ))))
 
  (define (dec-digit? char) (char-numeric? char))
  
  (define (dec-digit-to-int c) (- (char->integer c) zero-char-val))
  
  (define (parse-dec-char parse c)
    (let ((tag (car parse))
          (val (cadr parse)))
      (cond ((and (eq? tag 'dec) (dec-digit? c))
	     (let ((new-val (+ (* 10 val) (dec-digit-to-int c))))
	       ; ... fill in ...
	       (return `(dec ,new-val))
	     ))
            (else ; ... fill in ...
	     m-zero
	     ))))
  
  (define (parse-word-char parse c)
    (let ((tag (car parse))
          (word (cadr parse)))
      (cond ((and (eq? tag 'word) (char-alphabetic? c))
             (let ((new-val (string-append word (string c))))
	       ; ... fill in ...
	       (return `(word ,new-val))
	       ))
            (else ; ... fill in ...
	     m-zero
	     ))))

  ; extend the existing parse by a character
  ; it could be the character of a word, hex number or decimal number	    
  (define (parse-char parse c) 
    ; ... fill in body ...
    (m-plus (parse-word-char parse c) (parse-hex-char parse c) (parse-dec-char parse c))
    )
  
  ; the possible results when we start parsing
  (define initial-parses-m
    (m-plus (return '(hex 0)) (return '(dec 0)) (return '(word ""))))
 
  (define (fold-m m-fn init l)
    (if (null? l)
        (return init)
        (letM ((next-init (m-fn init (car l))))
              (fold-m m-fn next-init (cdr l)))))
  
  ; bind an initial parse
  ; fold parse-char over the string monadically 
  ; to process the entire string given that starting point
  (define (parse-string string)
    (letM ((init initial-parses-m))
          (fold-m parse-char init (string->list string))))

  (define (run-parse-tests)
    (map parse-string '("0" "100" "dead" "toast" "f00d")))

  (run-parse-tests)

  )


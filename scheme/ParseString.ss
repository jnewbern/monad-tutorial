(module ParseString scheme
  (require "List.ss" "Monad.ss")
  (provide parse-string)
  
  ; parse strings into hex values, decimal values or words
  ; possible results are (hex val) (dec val) (word string)
  
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
         ; to be filled in
         (return `(hex ,(+ (* 16 val) (hex-digit-to-int c)))))
        (else m-zero))))

  (define (dec-digit? char) (char-numeric? char))
  
  (define (dec-digit-to-int c) (- (char->integer c) zero-char-val))
  
  (define (parse-dec-char parse c)
    (let ((tag (car parse))
          (val (cadr parse)))
      (cond ((and (eq? tag 'dec) (dec-digit? c))
             (return `(dec ,(+ (* 10 val) (dec-digit-to-int c)))))
            (else m-zero))))
  
  (define (parse-word-char parse c)
    (let ((tag (car parse))
          (word (cadr parse)))
      (cond ((and (eq? tag 'word) (char-alphabetic? c))
             (return `(word ,(string-append word (string c)))))
            (else m-zero))))
     
  (define (parse-char parse c)
    (m-plus (parse-word-char parse c) (parse-hex-char parse c) (parse-dec-char parse c)))
  
   (define initial-parses-m
    (m-plus (return '(hex 0)) (return '(dec 0)) (return '(word ""))))
 
  (define (parse-string string)
    (letM ((init initial-parses-m))
          (fold-m parse-char init (string->list string))))
  
  (define (fold-m m-fn init l)
    (if (null? l)
        (return init)
        (letM ((next-init (m-fn init (car l))))
              (fold-m m-fn next-init (cdr l)))))
  )
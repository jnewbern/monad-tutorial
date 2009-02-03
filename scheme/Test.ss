(require (rename-in "Maybe.ss" [bind Maybe::bind] [return Maybe::return]))

(from-just (letM (s (return "Hello")) (return (string-append s "!"))))

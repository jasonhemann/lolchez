(load "parser.scm")
(load "int.scm")



(define read-line
  (lambda ()
    (let readc ([s ""]
                [c (read-char)])
      (cond
        [(eof-object? c) c]
        [(equal? c #\newline) s]
        [(readc (string-append s (string c))
                (read-char))]))))


;; REPL should track the last value of IT and take care of building complete 
;; expressions before handing it over to the interpreter. Parser will signal if 
;; it has parsed a complete expression or if it is waiting to find the end of 
;; a block.

(define lol-repl
  (lambda ()
    (display "LOL:p ")
    (let ([input (read-line)])
      (cond
        [(eof-object? input) (printf "BAI!~n")]
        [(= 0 (string-length input)) (lol-repl)]
        [else (let* ([parsed (parse input)]
                     [output (int parsed)])
                (if (not (eq? output (void)))
                  (begin (write output)
                         (newline)))
                (lol-repl))]))))

(load "parser.scm")
(load "int.scm")



(define read-line
  (lambda ()
    (let readc ([s ""]
                [c (read-char)])
      (if (or (equal? c #\newline)
              (eof-object? c))
        s
        (readc (string-append s (string c))
               (read-char))))))


;; REPL should track the last value of IT and take care of building complete 
;; expressions before handing it over to the interpreter. Parser will signal if 
;; it has parsed a complete expression or if it is waiting to find the end of 
;; a block.

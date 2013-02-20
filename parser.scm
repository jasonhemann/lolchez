(define split-string
  (lambda (p? str)
    (let loop ([i 0])
      (cond
        [(= i (string-length str)) (list str)]
        [(p? (string-ref str i))
         (cons (substring str 0 i)
               (split-string p? (substring str (add1 i) (string-length str))))]
        [else (loop (add1 i))]))))

(define words
  (lambda (str)
    (split-string char-whitespace? str)))

(define lines
  (lambda (str)
    (split-string (lambda (c) (equal? c #\newline)) str)))

(define parser
  (lambda (wds)
    (map (lambda (x) 
           (let ([n (string->number x)])
             (if n n (string->symbol x))))
         wds)))

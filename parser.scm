(define words
  (lambda (str)
    (let loop ([i 0])
      (cond
       [(= i (string-length str)) (list str)]
       [(char-whitespace? (string-ref str i))
	(cons (substring str 0 i)
	      (words (substring str (add1 i) (string-length str))))]
       [else
	(loop (add1 i))]))))

(define parser
  (lambda (wds)
    (map (lambda (x) 
           (let ([n (string->number x)])
             (if n n (string->symbol x))))
         wds)))

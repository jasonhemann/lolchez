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


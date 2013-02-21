;; Main parser

(define parse
  (lambda (str)
    (let ([syms (map (lambda (w)
                       (let ([n (string->number w)])
                         (if n n (string->symbol w))))
                     (words str))])
      (if (null? (cdr syms)) ; If parsing a single word,
        (car syms)           ; return just that symbol. Otherwise,
        syms))))             ; return the list of parsed symbols.



;; Some string-splitting functions

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

(define commas
  (lambda (str)
    (split-string (lambda (c) (equal? c #\,)) str)))

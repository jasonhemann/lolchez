;; Main parser

(define parse
  (lambda (str)
    (if (incomplete-line? str)
      (return-incomplete-obj str)
      (let ([syms (map (lambda (w)
                         (let ([n (string->number w)])
                           (if n n (string->symbol w))))
                       (fix-UP!! (words str)))])
        (if (null? (cdr syms)) ; If parsing a single word,
          (car syms)           ; return just that symbol. Otherwise,
          syms)))))            ; return the list of parsed symbols.

(define incomplete-line?
  (lambda (str)
    (and (> (string-length str) 3)
         (equal? "..." (substring str (- (string-length  str) 3) (string-length str))))))

(define incomplete?
  (lambda (parsed)
    (and (pair? parsed) (eq? 'incomplete-value (car parsed)))))

(define return-incomplete-obj
  (lambda (str)
    `(incomplete-value ,(substring str 0 (- (string-length str) 3)))))

(define fix-UP!!
  (lambda (wds)
    (fold-right (lambda (w wds)
                  (if (string-prefix? w "UP!!")
                    (cons "UP!!" (let ([arg (substring w 4 (string-length w))])
                                   (if (= 0 (string-length arg))
                                     wds
                                     (cons arg wds))))
                    (cons w wds)))
                '()
                wds)))

(define string-prefix?
  (lambda (str pre)
    (let loop ([i 0])
      (cond
        [(= i (string-length pre)) #t]
        [(= i (string-length str)) #f]
        [(eq? (string-ref str i) (string-ref pre i))
         (loop (add1 i))]
        [else #f]))))



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

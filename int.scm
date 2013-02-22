(load "pmatch.scm")

(define env
  (make-parameter '()))

(define extend-env
  (lambda (var val)
    (env (cons (cons var val) (env)))))

;; prime IT
(extend-env 'IT (box (void)))

(define change-env
  (lambda (var val)
    (set-box! (apply-env var) val)))

(define apply-env
  (lambda (x)
    (let ([kv (assq x (env))])
      (if kv
        (cdr kv)
        (begin
          (format (current-error-port) "NO FIND YR ~s!!!~n" x)
          (box (void)))))))

(define int
  (lambda (e)
    (let ([it (pmatch e
                [(,var R ,val) (change-env var val)]
                [(,n1 UP!!) (add1 (int n1))]
                [(,n1 UP!! ,n2) (+ (int n1) (int n2))]
                [(,n1 NERF ,n2) (- (int n1) (int n2))]
                [(,n1 TIEMZ ,n2) (* (int n1) (int n2))]
                [(,n1 OVAR ,n2) (/ (int n1) (int n2))]
                [(FUCK MICHAEL)
                 (begin (printf "0> LOL. RAGEQUIT. \n")
                        (int '(FUCK MICHAEL 1)))] 
                [(FUCK MICHAEL ,n)
                 (begin (printf "~s> LOL. RAGEQUIT. \n" n)
                        (int `(FUCK MICHAEL ,(add1 n))))]
                [,n (guard (number? n)) n]
                [,x (guard (symbol? x)) (unbox (apply-env x))]
                [(I HAZ A ,var ITS ,val) (guard (symbol? var))
                                         (extend-env var (box val))]
                [(I HAZ A ,var) (guard (symbol? var))
                                (extend-env var (box 'null))]
                [,invalid
                  (begin (format (current-error-port) "invalid syntax in:~n")
                         invalid)])])
      (change-env 'IT it)
      it)))

(load "pmatch.scm")

(define env
  (make-parameter '()))

(define extend-env
  (lambda (var val)
    (env (cons (cons var val) (env)))))

(define change-env
  (lambda (var val)
    (set-box! (apply-env var) val)))

(define apply-env
  (lambda (x)
    (cdr (assq x (env)))))

(define int
  (lambda (e)
    (pmatch e
      [(,var R ,val) (change-env var val)]
      [(,n1 UP ,n2) (+ n1 n2)]
      [(,n1 NERF ,n2) (- n1 n2)]
      [(,n1 TIEMZ ,n2) (* n1 n2)]
      [(,n1 OVAR ,n2) (/ n1 n2)]
      [(,FUCK ,MICHAEL) (guard (equal? FUCK 'FUCK) (equal? MICHAEL 'MICHAEL))
       (begin (printf "LOL. RAGEQUIT. \n")
              (int "FUCK MICHAEL"))] 
      [(,n) (guard (number? n)) n]
      [(,x) (guard (symbol? x)) (unbox (apply-env x))]
      [(I HAZ A ,var ITS ,val) (guard (symbol? var))
       (extend-env var (box val))]
      [(I HAZ A ,var) (guard (symbol? var))
       (extend-env var (box 'null))])))


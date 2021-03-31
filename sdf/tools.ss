

(define (starts-with? str . start)
  (define (start-with? str start)
    (cond
     [(string=? start "") #t]
     [(string=? str "") #f]
     [else (string=? (substring str 0 (string-length start)) start)]))
  (let ([len (length start)])
    (cond
     [(= len 0) #t]
     [(= len 1) (start-with? str (car start))]
     [else (or (start-with? str (car start))
               (apply starts-with? str (cdr start)))])))

;;;(starts-with? "gafgsdfhsfy" "dd" "gg" "aa" "gaf")




(define (sub-set? f subc c)
  (let loop ([lst subc])
    (cond [(null? lst) #t]
          [(memp (lambda (x) (f x (car lst))) c) (loop (cdr lst))]
          [else #f])))

;;(contain? eqv? '(c b) '(a b c d))
;;(contain? eqv? '(c 1) '(a b c d))

(define (lset= f . llst)
  (let ([n (length llst)])
    (cond [(= n 0) #t]
          [(= n 1) #t]
          [(= n 2) (and (sub-set?? f (cadr llst) (car llst))
                        (sub-set?? f (car llst) (cadr llst)))]
          [else (and (lset= f (list (car llst) (cadr llst)))
                     (apply lset= f (cddr llst)))])))

;; (lset= eqv? '(c a b) '(b c a) '(a b c))


(define append-map
  (lambda (f . args)
    (apply append (apply map f args))))

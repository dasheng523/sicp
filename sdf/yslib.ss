(define (compose . fs)
  (let ([n (length fs)])
    (assert (> n 0))
    (cond [(= n 1) (car fs)]
          [(= n 2) (let ([g (car fs)]
                         [f (cadr fs)])
                     (restrict-arity (lambda args
                                       (assert (= (length args) (get-arity f)))
                                       (call-with-values
                                           (lambda () (apply f args)) g))
                                     (get-arity g)))]
          [else (apply compose (compose (car fs) (cadr fs))
                       (cddr fs))])))

;;((compose (lambda (x) (list 'foo x)) (lambda (x) (list 'bar x))) 'z)
#;((compose
  (lambda (x y) (list x y))
  (lambda (x) (values 'b x))
  (lambda (x) (list 'a x)))
 'ys)

(define (identity x) x)






(define arity-table (make-eqv-hashtable))

(define (restrict-arity proc nargs)
  (hashtable-set! arity-table proc nargs)
  proc)

(define (get-arity proc)
  (or (hashtable-ref arity-table proc #f)
      (let ((mask (procedure-arity-mask proc)))
        (assert (> mask 0))
        (procedure-arity-min mask))))

(define (procedure-arity-min mask)
  (let find-num [(i 0)]
    (if (logbit? i mask)
        i
        (find-num (+ i 1)))))







(define (parallel-combine h f g)
  (compose h (parallel-apply f g)))

(define (parallel-apply f g)
  (let ([n (get-arity f)] [m (get-arity g)])
    (define (the-combination . args)
      (let [(t (length args))]
        (assert (and (= t n) (= t m)))
        (let-values [(fv (apply f args))
                     (gv (apply g args))]
          (apply values (append fv gv)))))
    (restrict-arity the-combination n)))
#;((parallel-combine list
                   (lambda (x y z) (values x y))
                   (lambda (a b c) (values a b c)))
 'a 'b 'c)


(define (spread-combine h f g)
  (compose h (spread-apply f g)))

(define (spread-apply f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (let-values ((fv (apply f (list-head args n)))
                     (gv (apply g (list-tail args n))))
          (apply values (append fv gv))))
      (restrict-arity the-combination t))))

#;((spread-combine list
                 (lambda (x y) (list 'foo x y))
                 (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)

#;((compose (lambda (a b) (list 'foo a b))
          (lambda (x) (values (list 'bar x)
                              (list 'baz x))))
 'z)

#;((spread-combine list
                 (lambda (x y) (values x y))
                 (lambda (u v w) (values u v w)))
 'a 'b 'c 'd 'e)


;; 使f的返回值是多值
(define (to-values f)
  (restrict-arity
   (lambda args
     (assert (= (length args) (get-arity f)))
     (apply values (apply f args)))
   (get-arity f)))

;; 组合多值
(define (compose-values f m)
  (compose f (to-values m)))


;; 是否是非负整数
(define exact-nonnegative-integer?
  (lambda (i)
    (and (positive? i) (exact? i))))



;;;;;;;;;;;;;   list 操作相关
(define (list-remove lst index)
  (let lp ((lst lst) (index index))
    (if (= index 0)
        (cdr lst)
        (cons (car lst) (lp (cdr lst) (- index 1))))))

(define (list-insert lst index value)
  (let lp ((lst lst) (index index))
    (if (= index 0)
        (cons value lst)
        (cons (car lst) (lp (cdr lst) (- index 1))))))

(define (list-replace lst n v)
  (assert (> (length lst) n))
  (let ([head (list-head lst n)] [tail (list-tail lst n)])
    (append head (cons v (cdr tail)))))



;; curry参数
(define (curry-argument i)
  (lambda args
    (lambda (f)
      (compose-values f (lambda (x) (list-insert args i x))))))

#;((((curry-argument 2) 'a 'b 'c) (lambda (x y z w) (list 'foo x y z w))) 'd)


;; curry多个参数
(define (curry-arguments . curryspec)
  (lambda args
    (lambda (f)
      (assert (= (get-arity f) (+ (length args) (length curryspec))))
      (compose-values f (make-curry args curryspec)))))

(define (make-curry args curryspec)
  (define (the-combination . as)
    (assert (= (length as) (length curryspec)))
    (let ([data (list-sort
                 (lambda (a b) (> (car a) (car b)))
                 (map (lambda (i v) (cons i v)) curryspec as))])
      (let loop ([data data] [rs args])
        (cond
         [(null? data) rs]
         [else (loop (cdr data) (list-insert rs
                                             (car (car data))
                                             (cdr (car data))))]))))
  (restrict-arity the-combination (length curryspec)))

;;((((curry-arguments 2) 'a 'b 'c) (lambda (x y z w) (list 'foo x y z w))) 'd)
;;((((curry-arguments 3 1) 'a 'b 'c) (lambda (x y z w s) (list 'foo x y z w s))) 'e 'd)
;;((((curry-arguments 1 3) 'a 'b 'c) (lambda (x y z w s) (list 'foo x y z w s))) 'd 'e)



;; 省略参数
(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (lambda (f)
    (compose-values f (restrict-arity (lambda args (list-remove args i)) (+ 1 (get-arity f))))))

#;(((discard-argument 2) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd)


;; 省略参数
(define (discard-arguments . spec)
  (lambda (f)
    (compose-values f (make-discard spec f))))
(define (make-discard spec f)
  (let ([s (+ (get-arity f) (length spec))])
    (define (the-combination . args)
      (assert (= (length args) s))
      (let loop ([spec (list-sort > spec)] [rs args])
        (cond
         [(null? spec) rs]
         [else (loop (cdr spec) (list-remove rs (car spec)))])))
    (restrict-arity the-combination s)))


;;(((discard-arguments 2) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd)
;;(((discard-arguments 2 3) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd 'u)
;;(((discard-arguments 3 2) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd 'u)




;; 排列参数
(define (permute-arguments . permspec)
  (lambda (f)
    (compose-values f (make-permutation permspec))))
(define (make-permutation permspec)
  (define (the-permuter . lst)
    (map (lambda (p) (list-ref lst p)) permspec))
  (restrict-arity the-permuter (length permspec)))

;;(((permute-arguments 1 2 0 3) (lambda (x y z w) (list 'foo x y z w))) 'a 'b 'c 'd)



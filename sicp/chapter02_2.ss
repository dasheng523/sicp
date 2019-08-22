(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))


(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)


;; 2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))


(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define aa (make-exponentiation '(+ x 1) 8))
(deriv aa 'x)


;; 2.57
(define (augend s)
  (let ((es (cddr s)))
    (cond ((eq? '() es) 0)
          ((eq? '() (cdr es)) (car es))
          (else (make-sum (car es)
                          (augend (cons '+ es)))))))


(define (multiplicand s)
  (let ((es (cddr s)))
    (cond ((eq? '() es) 1)
          ((eq? '() (cdr es)) (car es))
          (else (make-product (car es)
                              (multiplicand (cons '* es)))))))


(deriv '(* x y (+ x 3)) 'x)
(deriv '(* x x y) 'x)

;; 下面这种解法非常麻烦，是因为加入了一个数据类型，coll-exp。

(define (coll-exp? exp)
  (and (pair? exp)
       (let ((f (car exp)))
         (and (not (eq? f '+))
              (not (eq? f '*))))))

(define (make-sum a1 a2)
  (cond ((and (=number? a1 0) (not (coll-exp? a2))) a2)
        ((and (=number? a1 0) (coll-exp? a2)) (make-sum (car a2) (cdr a2)))
        ((or (=number? a2 0) (eq? a2 '())) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((coll-exp? a2)
         (make-sum a1 (make-sum (car a2) (cdr a2))))
        (else (list '+ a1 a2))))


(define (augend s)
  (let ((es (cddr s)))
    (cond ((eq? '() es) 0)
          (else (make-sum (car es) (cdr es))))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((and (=number? m1 1) (not (coll-exp? m2))) m2)
        ((and (=number? m1 1) (coll-exp? m2)) (make-product (car m2) (cdr m2)))
        ((or (=number? m2 1) (eq? m2 '())) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((coll-exp? m2)
         (make-product m1 (make-product (car m2) (cdr m2))))
        (else (list '* m1 m2))))

(make-product 'x (list 'x 'x 'y))

(define (multiplicand s)
  (let ((es (cddr s)))
    (cond ((eq? '() es) 1)
          (else (make-product (car es) (cdr es))))))




;; 2.58 a
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s)
  (car s))

(define (augend s)
  (caddr s))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list m1 '* m2))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier s)
  (car s))

(define (multiplicand s)
  (caddr s))


(deriv '(x + (3 * (x + (y + 2)))) 'x)


;; 2.58 b
;; 能。关键在于谓词和选择函数的设计。其实就是定义清楚概念。

(define (memq item x)
  (cond ((null? x) x)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (sum? s)
  (and (pair? s) (not (null? (memq '+ s)))))


(define (addend s)
  (define (mema x result)
    (cond ((null? x) result)
          ((eq? (car x) '+) result)
          (else (mema (cdr x)
                      (append result (list (car x)))))))
  (let ((rs (mema s '())))
    (if (null? (cdr rs))
        (car rs)
        rs)))


(define (augend s)
  (let ((rs (cdr (memq '+ s))))
    (if (null? (cdr rs))
        (car rs)
        rs)))


(define (product? x)
  (and (pair? x) (not (sum? x)) (eq? (cadr x) '*)))


(define (multiplier p)
  (car p))

(define (multiplicand p)
  (if (null? (cdr (cddr p)))
      (caddr p)
      (cddr p)))

(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x * 3 + (2 + x * y + 2)) 'x)

(product? (augend (list 'x '+ 3 '* (list 'x '+ 'y '+ 2))))
(list 'x '+ 3 '* (list 'x '+ 'y '+ 2))
(list 'x '* 3 '+ (list 'x '+ 'y '+ 2))


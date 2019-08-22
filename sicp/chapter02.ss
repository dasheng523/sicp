

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))




;;;;;;; 2.1.1

(define (make-rat n d)
  (cons n d))

(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (numer y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))


(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))


(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


(define one-half (make-rat 1 2))
(define one-thrid (make-rat 1 3))

(print-rat one-half)
(print-rat (add-rat one-half one-thrid))
(print-rat (mul-rat one-half one-thrid))



;;;;;;; exe 2.1
(define aone-half (make-rat -1 2))
(define aone-thrid (make-rat 1 -3))

(define (make-rat n d)
  (cond ((or (and (> n 0) (< d 0))
             (and (< n 0) (< d 0)))
         (make-rat (- n) (- d)))
        (else (cons n d))))

(print-rat (mul-rat aone-half aone-thrid))



;;;;;;; exe 2.2
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))


(define (midpoint-segment seg)
  (make-point (/ (+ (x-point (start-segment seg))
                    (x-point (end-segment seg)))
                 2)
              (/ (+ (y-point (start-segment seg))
                    (y-point (end-segment seg)))
                 2)))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(print-point
 (midpoint-segment (make-segment (make-point 3.0 5.0)
                                 (make-point 8.0 10.0))))




;;;;;;; exe 2.3
(define (make-rectangle a-point b-point)
  (cons a-point b-point))

(define (get-width ract)
  (abs (- (x-point (car ract))
          (x-point (cdr ract)))))


(define (get-height ract)
  (abs (- (y-point (car ract))
          (y-point (c(split-point (make-vect 0.5 0.0))dr ract)))))

(define (girth-rectangle ract)
  (* 2 (+ (get-width ract)
          (get-height ract))))


(girth-rectangle (make-rectangle (make-point -1 8)
                                 (make-point 5 10)))


(define (make-rectangle-other width height)
  (make-rectangle (make-point width height)
                  (make-point 0 0)))

(girth-rectangle (make-rectangle-other 10 20))



;;;;;;;;; xinde
;; 数据究竟是什么？按课本所说，数据其实就是满足一些限定条件的函数集合。这是很简单得形式化定义，对日常开发非常有帮助。
;; 序对这种数据的定义不就是由三个函数组成吗？这三个函数满足了一个条件，如果x是(cons a b)，那么a=(car x),b=(cdr x)。
;; 有了清晰的数据形式化定义，可以构建更加庞大的高层数据定义，还可以使用底层数据定义。




;;;;;;; exe 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))


(car (cons x y))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
(lambda (x y) x)
x


(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons x y))
((lambda (m) (m x y)) (lambda (p q) q))
((lambda (p q) q) x y)
(lambda (x y) y)
y




;;;;;;; exe 2.5
(define (cons-a x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car-a z)
  (define (car-iter i num)
    (cond ((= (remainder num 2) 0)
           (car-iter (+ i 1) (/ num 2)))
          (else i)))
  (car-iter 0 z))


(define (cdr-a z)
  (define (cdr-iter i num)
    (cond ((= (remainder num 3) 0)
           (cdr-iter (+ i 1) (/ num 3)))
          (else i)))
  (cdr-iter 0 z))

(car-a (cons-a 3 7))
(cdr-a (cons-a 3 7))




;;;;;;; exe 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)

(lambda (f) (lambda (x) (f ((lambda (x) x) x))))

;; 所以 one 是
(define one (lambda (f) (lambda (x) (f x))))


(add-1 one)
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

;; 所以 two 是
(define two (lambda (f) (lambda (x) (f (f x)))))

;; one，two的不同在于多调用了一次f，所以加法的实现，就是使得f的次数增加。
;; 具体的说，
;; two: (lambda (f) (lambda (x) (f (f x))))
;; one: (lambda (f) (lambda (x) (f x)))
;; 而three： (lambda (f) (lambda (x) (f (f (f x)))))
;; 其实就是将one作为参数x传入给two进行调用即可。

(define (add a b)
  (lambda (f) (lambda (x) ((f a) (f b)))))

;; 证明 1 + 2 = 3
(lambda (f)
  (lambda (x)
    ((f (lambda (f) (lambda (x) (f x))))
     (f (lambda (f) (lambda (x) (f (f x))))))))

;; 代换1
(lambda (f)
  (lambda (x)
    ((lambda (x) (f x))
     (lambda (x) (f (f x))))))


;; 代换2
(lambda (f)
  (lambda (x)
    (f (f (f x)))))

;; 这刚好是three



(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;;;;;;; exe 2.7
(define (make-interval a b)
  (cons a b))

(define (lower-bound a)
  (car a))

(define (upper-bound a)
  (cdr a))


;; test
(add-interval (make-interval 5 5.6)
              (make-interval 6 6.2))

(mul-interval (make-interval 5 5.6)
              (make-interval 6 6.2))

(div-interval (make-interval 5 5.6)
              (make-interval 6 6.2))


;;;;;;; exe 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(sub-interval (make-interval 5 5.6)
              (make-interval 6 6.2))

(sub-interval (make-interval 8 10.0)
              (make-interval 2 5))



;;;;;;; exe 2.9

;; 计算区间宽度函数
(define (width-interval x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

;; 区间和的宽度
(width-interval (add-interval x y))

;; 等价于
(width-interval (make-interval (+ (lower-bound x) (lower-bound y))
                               (+ (upper-bound x) (upper-bound y))))

;; 等价于
(let (z (make-interval (+ (lower-bound x) (lower-bound y))
                       (+ (upper-bound x) (upper-bound y))))
  (/ (- (upper-bound z)
        (lower-bound z))
     2))

;; 等价于
(/ (- (+ (upper-bound x) (upper-bound y))
      (+ (lower-bound x) (lower-bound y)))
   2)

;; 等价于
(+ (/ (- (upper-bound x)
         (lower-bound x))
      2)
   (/ (- (upper-bound y)
         (lower-bound y))
      2))

;; 等价于
(+ (width-interval x)
   (width-interval y))

;; 所以
(width-interval (add-interval x y))
;; 等价于
(+ (width-interval x)
   (width-interval y))


;; 同理进行转换可得
(width-interval (sub-interval x y))
;; 等价于
(- (width-interval x)
   (width-interval y))


;;;;;;;;;;;;;;
(width-interval (mul-interval (make-interval 5 5.6)
                              (make-interval 6 6.2)))
;;2.36

(* (width-interval (make-interval 5 5.6))
   (width-interval (make-interval 6 6.2)))
;; 0.03


(width-interval (div-interval (make-interval 5 5.6)
                              (make-interval 6 6.2)))
;; 0.634

(/ (width-interval (make-interval 5 5.6))
   (width-interval (make-interval 6 6.2)))
;; 3

;; 可见对于乘除运算，情况并非如此。



;;;;;;; exe 2.10
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
      (error "not allow")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(div-interval (make-interval 8 8)
              (make-interval -4 9))




;;;;;;; exe 2.11
(define (mul-interval x y)
  (let ((x-lower (lower-bound x))
        (x-upper (upper-bound x))
        (y-lower (lower-bound y))
        (y-upper (upper-bound y)))
    (cond ((or (and (>= x-lower 0) (>= y-lower 0) (>= x-upper 0) (>= y-upper 0))
               (and (<= x-lower 0) (>= y-lower 0) (>= x-upper 0) (>= y-upper 0))
               (and (>= x-lower 0) (<= y-lower 0) (>= x-upper 0) (>= y-upper 0)))
           (make-interval (* x-lower y-lower) (* x-upper y-upper)))

          ;; 两个区间都跨0轴的情况，这里计算了4次乘法
          ((and (<= x-lower 0) (<= y-lower 0) (>= x-upper 0) (>= y-upper 0))
           (make-interval (min (* x-upper y-lower) (* x-lower y-upper))
                          (max (* x-upper y-upper) (* x-lower y-lower))))

          ((and (<= x-lower 0) (>= y-lower 0) (<= x-upper 0) (>= y-upper 0))
           (make-interval (* x-lower y-upper) (* x-upper y-lower)))
          ((and (>= x-lower 0) (<= y-lower 0) (>= x-upper 0) (<= y-upper 0))
           (make-interval (* y-lower x-upper) (* y-upper x-lower)))

          ((and (<= x-lower 0) (<= y-lower 0) (<= x-upper 0) (<= y-upper 0))
           (make-interval (* x-upper y-upper) (* x-lower y-lower)))
          ((and (<= x-lower 0) (<= y-lower 0) (<= x-upper 0) (>= y-upper 0))
           (make-interval (* x-lower y-upper) (* x-lower y-lower)))
          ((and (<= x-lower 0) (<= y-lower 0) (>= x-upper 0) (<= y-upper 0))
           (make-interval (*
                           x-upper y-lower) (* x-lower y-lower))))))



(mul-interval (make-interval
               8 10)
              (make-interval -4 9))




(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


;;;;;;; exe 2.12
(define (make-center-percent c p)
  (make-interval (* c (- 1 p)) (* c (+ 1 p))))

(define (percent i)
  (- 1 (/ (lower-bound i) (center i))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(percent (make-center-percent 8 0.02))



;;;;;;; exe 2.13
;; 设两个区间为
;; (make-center-percent c1 w1)
;; (make-center-percent c2 w2)

;; 两个区间相乘所得的百分比误差是
;; (percent (mul-interval (make-center-percent c1 w1) (make-center-percent c2 w2)))

;; 只考虑整数进行简化，且误差不大于1，转换后：
;; (percent (make-interval (* (* c1 (- 1 w1)) (* c2 (- 1 w2))) (* (* c1 (+ 1 w1)) (* c2 (+ 1 w2)))))
;; 转换后：
;; (percent (make-interval (* c1 c2 (- 1 w1) (- 1 w2)) (* c1 c2 (+ 1 w1) (+ 1 w2))))
;; 转换后：
;; (- 1 (/ (* c1 c2 (- 1 w1) (- 1 w2)) (/ (+ (* c1 c2 (- 1 w1) (- 1 w2)) (* c1 c2 (+ 1 w1) (+ 1 w2))) 2)))
;; 转换后：
;; (- 1 (/ (* c1 c2 (- 1 w1) (- 1 w2)) (/ (* c1 c2 (+ (* (- 1 w1) (- 1 w2)) (* (+ 1 w1) (+ 1 w2)))) 2)))
;; 转换后:
;; (- 1 (/ (* c1 c2 (- 1 w1) (- 1 w2)) (* c1 c2 (+ 1 (* w1 w2)))))
;; 转换后:
;; (- 1 (/ (* (- 1 w1) (- 1 w2)) (1 + (* w1 w2))))
;; 转换后：
;; (/ (+ w1 w2) (+ 1 (* w1 w2)))
;; 所以乘法后的误差是
;; (/ (+ w1 w2) (+ 1 (* w1 w2)))

;; 验证一下
(percent (mul-interval (make-center-percent 5 0.6) (make-center-percent 9 0.3)))  ;0.7627
(/ (+ 0.6 0.3) (+ 1 (* 0.6 0.3)))  ; 0.7627
;; 验证成功



;;;;;;; exe 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(par1 (make-interval 8 9)
      (make-interval 10 29))

(par2 (make-interval 8 9)
      (make-interval 10 29))

;; -_-!! 结果完全不一样。

(define one (make-interval 1 1))
(0.11111 0.125)

;; 取倒数是没问题的
(div-interval one (make-interval 10 29))
(div-interval one (div-interval one (make-interval 8 9)))

(div-interval one (mul-interval (make-interval 25 100) (make-interval 8 9)))

;; a * (b + c) , a*b+a*c 这两个表达式也相等
(add-interval (mul-interval (make-interval 2 3) (make-interval 25 100))
              (mul-interval (make-interval 2 3) (make-interval 8 9)))

(mul-interval (make-interval 2 3)
              (add-interval (make-interval 25 100)
                            (make-interval 8 9)))


;; (a + b) / c  a/c + b/c 这两个表达式也相等
(add-interval (div-interval (make-interval 25 100) (make-interval 2 3))
              (div-interval (make-interval 8 9) (make-interval 2 3)))

(div-interval (add-interval (make-interval 25 100)
                            (make-interval 8 9))
              (make-interval 2 3))

;; 下面表达式相等
(add-interval (mul-interval (make-interval 5 6) (make-interval 8 9))
              (mul-interval (make-interval 5 6) (make-interval 3 9)))

(mul-interval (make-interval 5 6)
              (add-interval (make-interval 8 9) (make-interval 3 9)))


;; 如果有负数参与，表达式不相等
(add-interval (mul-interval (make-interval -5 6) (make-interval -8 9))
              (mul-interval (make-interval -5 6) (make-interval 3 9)))

(mul-interval (make-interval -5 6)
              (add-interval (make-interval -8 9) (make-interval 3 9)))



;; 但下面两个表达式不相等
(sub-interval (mul-interval (make-interval 5 6) (make-interval 8 9))
              (mul-interval (make-interval 5 6) (make-interval 3 9)))

(mul-interval (make-interval 5 6)
              (sub-interval (make-interval 8 9) (make-interval 3 9)))


;; 下面的表达式也不相等
(sub-interval (div-interval (make-interval 25 100) (make-interval 2 3))
              (div-interval (make-interval 8 9) (make-interval 2 3)))

(div-interval (sub-interval (make-interval 25 100)
                            (make-interval 8 9))
              (make-interval 2 3))



;; 分子分母都除 (make-interval 2 3) 表达式结果不相等
(div-interval (div-interval (mul-interval (make-interval 25 100) (make-interval 2 3)) (make-interval 2 3))
              (div-interval (add-interval (make-interval 25 100) (make-interval 2 3)) (make-interval 2 3)))

;; 分子分母都乘 (make-interval 2 3) 表达式结果不相等
(div-interval (mul-interval (mul-interval (make-interval 25 100) (make-interval 2 3)) (make-interval 2 3))
              (mul-interval (add-interval (make-interval 25 100) (make-interval 2 3)) (make-interval 2 3)))

;; 除法有问题
(div-interval (make-interval 2 3)
              (make-interval 2 3))

(div-interval (make-interval 4 6)
              (make-interval 4 6))


;; 如果误差很小的情况，相除的结果会趋于1，反之，相除的结果误差会很大。
(div-interval (make-center-percent 2 0.003)
              (make-center-percent 2 0.003))


(div-interval (make-center-percent 2 0.003)
              (make-center-percent 2 0.002))



;;;;;;; exe 2.15

;; 当误差很小的时候，两个表达式的结果是非常相近的。
(div-interval (mul-interval (make-center-percent 25 0.0002) (make-center-percent 10 0.0005))
              (add-interval (make-center-percent 25 0.0002) (make-center-percent 10 0.0005)))

(div-interval one (add-interval (div-interval one (make-center-percent 25 0.0002))
                                (div-interval one (make-center-percent 10 0.0005))))

;; 但是误差加大的时候，par2的误差增长得更慢，所以par2的技术结果更精确。
(div-interval (mul-interval (make-center-percent 25 0.2) (make-center-percent 10 0.5))
              (add-interval (make-center-percent 25 0.2) (make-center-percent 10 0.5)))

(div-interval one (add-interval (div-interval one (make-center-percent 25 0.2))
                                (div-interval one (make-center-percent 10 0.5))))

;; 所以一个公式如果使得误差增长更慢，那么这个公式算出的答案将是更加精确。而要确保误差增长慢，较好的做法是减少非确定性的变量出现次数。

;; Alyssa说得对。



;;;;;;; exe 2.16
;; 加减乘除运算中，两个不同的非确定变量进行加减乘除时，都会导致误差发生变化，从而使得计算有偏差。不同的表达式，会有不同的非确定量计算次数，所以即使代数表达式等价，也将得到不同的结果。
;; 更一般的解释是，如果表达式计算存在一些误差值，计算误差值更低的算法将会更精确。
;; 而减少误差最好的办法是减少重复非确定性表达式。
;; 涉及到多项式的拆分。
;; 太难做到了，除非不进行计算，而是先进行多项式运算，得出最简的多项式，再进行求值运算。否则误差依然非常大。
;; 比如一个运算，(R1*R1 + 2*R1*R2 + R2*R2) / (R1 + R2)，其实等价于 R1 + R2，所以只需要计算R1+R2的结果即可，如果对前面的表达式进行计算，必然会增加大量误差。但是如何将前面的表达式用程序换算成 R1+R2 是一件非常难的事情。
;; 能否做得到，主要依赖于多项式的研究。后面章节有讲到多项式的抽象，到时候再回头看看。


;; 以下是一些计算过程。
(div-interval (mul-interval (make-center-percent 2 0.00006) (make-center-percent 2 0.3))
              (make-center-percent 2 0.3))

(/ (+ w1 w2) (+ 1 (* w1 w2)))

(define ss (/ (+ 0.00006 0.3) (+ 1 (* 0.00006 0.3))))

(make-center-percent 4 ss)

ss
0.3

(/ (+ ss (/ 1 0.3)) (+ (* ss (/ 1 0.3))))
3.63

(percent (div-interval (make-center-percent 2 0.3)
                       (make-center-percent 1 0.6)))

(percent (mul-interval (make-center-percent 2 0.3)
                       (make-center-percent 1 0.6)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.2.1 序列的表示

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(list-ref (list 1 4 9 16 25) 3)


(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))


(length (list 1 3 5 7 9))


(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ count 1))))
  (length-iter items 0))

(length (list 2 4 6 8))


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


(append (list 1 2 3 4 5) (list 40 2 4))




;;;;;;; exe 2.17
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

(last-pair (list 1 0 9 0))


;;;;;;; exe 2.18
(define (reverse items)
  (if (null? (cdr items))
      (list (car items))
      (append (reverse (cdr items)) (list (car items)))))



;;;;;;; exe 2.19
(define us-coins (list 50 25 10 5 1))

(define (count-change amount)
  (cc amount us-coins))


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(count-change 100)



;;;;;;; exe 2.20
(define (same-parity a . ls)
  (define (check type num)
    (or (and (= type 0)
             (= (remainder num 2) 0))
        (and (= type 1)
             (= (remainder num 2) 1))))
  (define (parity-fn type rs)
    (if (null? (cdr rs))
        (if (check type (car rs))
            (list (car rs))
            (list))
        (if (check type (car rs))
            (cons (car rs) (parity-fn type (cdr rs)))
            (parity-fn type (cdr rs)))))
  (parity-fn (remainder a 2) ls))


(same-parity 1 2 3 4 5 6 7 8)

(same-parity 22 2 3 4 5 6 7 8)




;;;;;;; exe 2.21
(define (square-list items)
  (if (null? items)
      (list)
      (cons (* (car items) (car items))
            (square-list (cdr items)))))


(square-list (list 1 2 3 4))


(define (square-list items)
  (map (lambda (x) (* x x))
       items))

(square-list (list 1 2 3 4))



;;;;;;; exe 2.22
;; 第一个算法(squere-list (list 2 3 4 5)) 的迭代过程如下:
(iter (list 2 3 4 5) nil)
(iter (list 3 4 5) (list 4))
(iter (list 4 5) (list 9 4))
(iter (list 5) (list 16 9 4))
(iter nil (list 25 16 9 4))
(list 25 16 9 4)
;; 所以主要是cons导致了顺序问题，它把新生成的项放到了第一项。


;; 第二个算法的顺序是没问题，但是生成的结果是嵌套的序列。主要原因是cons的第一个参数应该是元素，而不应该是一个序列。



;;;;;;; exe 2.23
(define (for-each dofn items)
  (define (do-something)
    (dofn (car items))
    (for-each dofn (cdr items)))
  (if (null? items)
      null
      (do-something)))


(for-each (lambda (x) (newline) (display x))
          (list 1 3 4 5))



;;;;;;; exe 2.24
(list 1 (list 2 (list 3 4)))
;;解释器打印 (1 (2 (3 4)))

;; 文本不好画图，用纸来画了。



;;;;;;; exe 2.25
(define list1 (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr list1)))))

(define list2 (list (list 7)))
(car (car list2))

(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3))))))))))))


;;;;;;; exe 2.26
(append (list 1 2 3) (list 4 5 6))
(cons (list 1 2 3) (list 4 5 6))
(list (list 1 2 3) (list 4 5 6))



;;;;;;; exe 2.27
(define (deep-reverse x)
  (cond ((null? x) null)
        ((not (pair? x)) x)
        (else (list (deep-reverse (car (cdr x)))
                         (deep-reverse (car x))))))


(define (cons-null a b)
  (if (null? a)
      (cons b)
      (cons a b)))


(deep-reverse (list (list 1 2) (list 3 4)))
(deep-reverse (list 2 (list 3 4)))



;;;;;;; exe 2.28
(define (fringe x)
  (cond ((null? x) null)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                    (fringe (car (cdr x)))))))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))



;;;;;;; exe 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (car (cdr m)))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (car (cdr b)))


(define (total-weight m)
  (define (branch-weight b)
    (cond
     ((null? b) 0)
     ((not (pair? (branch-structure b))) (branch-structure b))
     (else (total-weight (branch-structure b)))))
  (cond
   ((null? m) 0)
   ((or (not (pair? (left-branch m)))
        (not (pair? (right-branch m))))
    (error "error branch"))
   (else (+ (branch-weight (left-branch m))
            (branch-weight (right-branch m))))))

(total-weight (make-mobile (make-branch 10 1)
                           (make-branch 12 9)))

(total-weight (make-mobile (make-branch 10
                                        (make-mobile (make-branch 2 9)
                                                     (make-branch 9 9)))
                           (make-branch 12 9)))


(define (is-balance m)
  (define (branch-weight b)
    (cond
     ((null? b) 0)
     ((not (pair? (branch-structure b))) (branch-structure b))
     (else (if (is-balance (branch-structure b))
               (total-weight (branch-structure b))
               0))))
  (define (branch-force b)
    (* (branch-length b)
       (branch-weight b)))
  (let ((left-force (branch-force (left-branch m)))
        (right-force (branch-force (right-branch m))))
    (and (not (= 0 left-force))
         (not (= 0 right-force))
         (= right-force left-force))))

(is-balance (make-mobile (make-branch 10
                                      (make-mobile (make-branch 2 2)
                                                   (make-branch 2 2)))
                         (make-branch 40 1)))



;; 只需要修改一下的代码即可，这里其实已经实现了隔离层。
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cdr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cdr b))



;;;;;;; exe 2.30
(define (square-tree t)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       t))


(square-tree
 (list 1
       (list 2 (list 3 4) 5)))


(define (square-tree t)
  (cond ((null? t) null)
        ((not (pair? t)) (square t))
        (else (cons (square-tree (car t))
                    (square-tree (cdr t))))))


(square-tree
 (list 1
       (list 2 (list 3 4) 5)))




;;;;;;; exe 2.31
(define (tree-map prod t)
  (cond ((null? t) null)
        ((not (pair? t)) (prod t))
        (else (cons (square-tree (car t))
                    (square-tree (cdr t))))))


(define (square-tree tree)
  (tree-map square tree))


(square-tree
 (list 1
       (list 2 (list 3 4) 5)))



;;;;;;; exe 2.32
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))


(subsets (list 1 2 3))

;; 这个算法的思路是，所有的子集等于第一个元素和其他元素的子集合并起来。





;;;;;;; 2.2.3 序列作为一种约定的界面
;; 数据抽象解决的是底层数据的封装问题，使得程序保持很好的弹性，避免数据表示的细节纠缠。
;; 还有一种设计原理，使用约定界面。
;; 我理解的约定界面，就是每个函数入参出参的数据结构是一致的，这样就可以使用有限的函数构建无数功能了。

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(accumulate + 0 (list 1 2 3 4))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 1 10)

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))


(enumerate-tree (list 1 (list 2 (list 3 4) 5)))




;;;;;;; exe 2.33
(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              null
              sequence))

(map (lambda (x) (+ x 1)) (list 12 2 5))


(define (append seq1 seq2)
  (accumulate cons
              seq1
              seq2))

(append (list 12 34 65)
        (list 1 3 5))




;;;;;;; exe 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

(+ 1 (* 3 2) (* 5 2 2 2) (* 2 2 2 2 2))



;;;;;;; exe 2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (cond ((null? x) 0)
                           ((not (pair? x)) 1)
                           (else (count-leaves x))))
                   t)))

(count-leaves (list (list 1 2 4)
                    9
                    (list 3 5 (list 1))))



;;;;;;; exe 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


(accumulate-n + 0
              (list (list 1 2 3)
                    (list 4 5 6)
                    (list 7 8 9)
                    (list 10 11 12)))


;;;;;;; exe 2.37


(define (dot-product v w)
  (accumulate + 0 (map * v w)))


(dot-product (list 1 2 3)
             (list 2 3 4))


(define (matrix-*-vector m v)
  (map (lambda (item) (accumulate + 0 (map * v item))) m))

(matrix-*-vector (list (list 1 2 3)
                       (list 4 5 6)
                       (list 7 8 9)
                       (list 7 8 9))
                 (list 1 2 1))


(define (transpose mat)
  (accumulate-n cons (list) mat))

(transpose (list (list 1 2 3)
                 (list 4 5 6)
                 (list 7 8 9)
                 (list 10 11 12)))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
           (matrix-*-vector cols v)) m)))


(matrix-*-matrix (list (list 1 2 3)
                       (list 4 5 6)
                       (list 7 8 9))
                 (list (list 1 2 3)
                       (list 4 5 6)
                       (list 7 8 9)))


;;;;;;; exe 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result)
              (cdr rest))))
  (iter initial sequence))


(fold-left / 1 (list 1 2 3))
(fold-right / 1 (list 1 2 3))


;; 如果要产生同样结果，op的参数顺序不能做任何要求，不管参数顺序如何，都能保证op的结果一样。



;;;;;;; exe 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (cons x y)) null sequence))

(reverse (list 1 2 3 4))


(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

(reverse (list 1 2 3 4))


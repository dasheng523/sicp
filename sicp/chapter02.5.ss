;; 通用型操作的系统

;; TODO 如果使用消息传递方式实现呢？

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; 语言算术包
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))

  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))

  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  'done)



(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; 有理数算术包
(define (install-rational-package)
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

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

  (define (tag x) (attach-tag 'rational x))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))

  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))

  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))

  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


;; 复数算术包
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))

  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))

  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))

  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))



;; 2.78
;; 既然没有complex数据类型的选择函数，那么自然就应该在算术包里定义出来。
;; 而magitude的实现本来就是通用型的，所以无需关注它如何实现，可以直接调用它即可。
;; 只是神奇的事情就发生在层层转换，最终完成了结果。
;; 要很好掌握这种技巧，就必须牢记当前定义的类型到底是什么，依赖什么下层界面。

(magnitude z)

;; 转换1
((lambda (z) (apply-generic 'magnitude z)) z)
;; 得到
(apply-generic 'magnitude z)
;; 继续转换
type-tags (complex)
proc magnitude
(apply magnitude (cons 'rectangular (cons 3 4)))
;; 也就是下面的调用，非常神奇
(magnitude (cons 'rectangular (cons 3 4)))

;; 继续转换
(apply-generic 'magnitude (cons 'rectangular (cons 3 4)))
;; 最后被分派到直角坐标的magnitude
type-tags: (rectangular)
proc: 直角坐标的magnitude
(magnitude-rectangular (cons 3 4))

;; 所以执行了两次apply-generic
;; 调用过程如上。
;; 给我的感觉是lamdba演算真的是太神奇了，里面的各种代换都非常巧妙。


;; 2.78
(define (attach-tag type-tag contents)
  (if (or (number? contents) (symbol? contents))
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((or (number? datum) (symbol? datum)) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((or (number? datum) (symbol? datum)) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))


(define (install-scheme-number-package)

  (put 'add '(scheme-number scheme-number)
       +)

  (put 'sub '(scheme-number scheme-number)
       -)

  (put 'mul '(scheme-number scheme-number)
       *)

  (put 'div '(scheme-number scheme-number)
       /)

  (put 'make 'scheme-number
       (lambda (x) x))

  'done)


;; 2.79
(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  ;; ...
  (put 'equ? '(scheme-number scheme-number) eq?)

  'done)


(define (install-rational-package)
  ;; ...
  (define (equ? a b)
    (= (/ (numer a) (denom a))
       (/ (numer b) (denom b))))


  (put 'equ? '(rational rational) equ?)

  'done)


(define (install-complex-package)
  ;; ...
  (define (equ? a b)
    (and (= (real-part a)
            (real-part b))
         (= (imag-part a)
            (imag-part b))))

  (put 'equ? '(complex complex) equ?)

  'done)


;; 2.80
(define (install-scheme-number-package)
  ;; ...
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))

  'done)


(define (install-rational-package)
  ;; ...
  (define (=zero? x)
    (= (numer x) 0))


  (put '=zero? '(rational) =zero?)

  'done)


(define (install-complex-package)
  ;; ...
  (define (=zero? z)
    (and (= (real-part z) 0)
         (= (imag-part z) 0)))

  (put '=zero? '(complex) =zero?)

  'done)



;; 2.5.2 不同类型数据的组合
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)


(define (apply-generic op .args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types")))))
              (error "No method for these types"))))))



;; 2.81
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))

(define (install-scheme-number-package)
  ;; ...
  (put 'exp '(scheme-number scheme-number)
       (lambda x y) (tag (expt x y)))

  'done)

;; a,b
;; 应该是死循环了，什么结果也得不到。
;; 主要是因为类型没有发生任何改变，最终触发t1->t2条件，又已相同的参数调用了apply-generic。

;; c
(define (apply-generic op .args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types")
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types"))))))
              (error "No method for these types"))))))


;; 2.82
;; 当所有参数的类型都是一样的时候，这种策略将会因为没有同类型强制过程而导致失败。
;; 所以要推广到更一般的方法，先对参数类型进行由下到上的排序，并去除相同项，排序完成之后，第一项就是最通用的类型。如果结果只有一项，就不需要进行强制。否则，将其他的类型转化为结果的第一项。


;; 2.83
(define (raise x)
  (apply-generic 'raise x))


(define (install-scheme-number-package)
  ;; ...
  (put 'raise '(scheme-number)
       (lambda x) (make-rational x 1)))

(define (install-rational-package)
  ;; ...
  (put 'raise '(rational)
       (lambda x) (make-real-number x)))

(define (install-real-number-package)
  ;; ...
  (put 'raise '(real-number)
       (lambda x) (make-complex-from-real-imag x 0)))

;; 2.84

(define type-tower (list 'scheme-number 'rational 'real-number 'complex))

(define (higher? type1 type2)
  (define (index-tower type ls)
    (cond ((null? ls) (error "type not found"))
          ((eq? type (car ls)) 0)
          (else (+ 1 (index-tower type (cdr ls))))))
  (> (index-tower type1 type-tower)
     (index-tower type2 type-tower)))

(define (raise-to-type data target-type)
  (let ((type (type-tags data)))
    (cond ((eq? type target-type) data)
          (else (raise-to-type (raise data) target-type)))))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types")
                    (if (higher? type1 type2)
                        (apply-generic op a1 (raise-to-type a2 type2))
                        (apply-generic op (raise-to-type a1 type2) a2))))
              (error "No method for these types"))))))

;; 用这个方案，如果增加新类型，只需要在type-tower增加新类型，并修改新类型下一层的类型的raise实现


;; 2.85
(define (drop x)
  (if (dropable? x)
      (drop (project x))
      x))

(define (dropable? x)
  (equ? x (raise (project x))))

(define (project x)
  (apply-generic 'project x))


(define (install-rational-package)
  ;; ...
  (put 'project '(rational)
       (lambda x) (numer x)))

(define (install-real-number-package)
  ;; ...
  (put 'project '(real-number)
       (lambda x) (make-rational x 1)))

(define (install-complex-package)
  ;; ...
  (put 'project '(complex)
       (lambda x) (real-part x)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types")
                    (drop
                     (if (higher? type1 type2)
                         (apply-generic op a1 (raise-to-type a2 type2))
                         (apply-generic op (raise-to-type a1 type2) a2)))))
              (error "No method for these types"))))))


;; 2.86
;; 这问题有点大，需要的改动还是不少的。
;; 因为以前的构造函数只支持数字，而现在需要支持更多类型，所以安装包的几乎所有实现都需要改。
;; 而以前的make-from-real-imag，make-from-mag-ang承诺了只要给两个参数就可以构造复数，而且这些复数能操作安装包的所有操作，所以安装包外部的操作不需要修改。但由于要支持其他类型的构造，需要使用sin,cos，所以需要在其他安装包上支持这个函数的实现，这是添加操作，也不需要修改原有实现。
;; 由于依赖了rectangular,polar两种底层实现，所以还需考察是否已经支持了数字外的其他类型，如果支持了，那么复数安装包就好写了。可以看到，rectangular,polar的一些函数需要重写，以适配其他类型。修改的办法是将原来的- + * / 换成通用型过程。如还依赖其他运算，则需要将这些预算换成通用型操作，如果没有，那就把实现出来。
;; 如果rectangular,polar支持了其他类型，那么复数包的默认数值操作函数改成通用型的函数即可，如下：

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (mul (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (div (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))

  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))

  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))

  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)

;; 下面是polar包的实现:
(define (install-polar-package)
  (define (real-part z)
    (mul (magnitude z) (cos (angle z))))

  (define (imag-part z)
    (mul (magnitude z) (sin (angle z))))

  (define (magnitude z)
    (car z))

  (define (angle z)
    (cdr z))

  (define (make-from-real-imag x y)
    (cons (sqrt (add (square x) (square y)))
          (atan y x)))

  (define (make-from-mag-ang r a)
    (cons r a))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; 由于这个包依赖了sqrt cos sin atan，这些函数只支持单一数值，为了能通用化，还需要在所有包中加入这些函数的实现，并基于apply-generic编写通用的函数。

;; 太多东西要写了。。。


;; 2.5.3 符号代数

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))

  (define (variable p)
    (car p))

  (define (term-list p)
    (cdr p))

  (define (variable? x) (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var")))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var")))


  (define (tag p) (attach-tag 'polynomial p))

  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))

  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
)


(define (add-terms L1 L2)
  (cond ((empty-terms L1) L2)
        ((empty-terms L2) L2)
        (else
         (let ((t1 (first-terms L1))
               (t2 (first-terms L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-terms L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist L)
      (the-empty-termlist)
      (adjoin-term (make-term (add (order t1)
                                 (order (first-terms L)))
                            (mul (coeff t1)
                                 (coeff (first-terms L))))
                   (mul-term-by-all-terms t1 (rest-terms L)))))


(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())

(define (first-terms term-list)
  (car term-list))

(define (rest-terms term-list)
  (cdr term-list))

(define (empty-termlist term-list)
  (null? term-list))

(define (make-term order coeff)
  (list order coeff))

(define (order term)
  (car term))

(define (coeff term)
  (cadr term))

(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))



;; 2.87

(define (install-polynomial-package)
  (define (=zero-termlist? termlist)
    (cond ((empty-termlist termlist) #t)
          (and
           (=zero? (coeff (first-terms termlist)))
           (=zero-termlist? (rest-terms termlist)))))
  (define (=zero? L)
    (=zero-termlist? (term-list L)))

  (put '=zero? 'polynomial =zero?))


;; 2.88
;; 需要在所有类型包中实现一个求负过程，然后定义一个通用的求负操作。
(define (negate x)
  (apply-generic 'negate x))

;; 这里只实现多项式的求负过程，其他的类型求负操作比较简单，就不写了。
;; 多项式求负过程，就是将每一项的项数求负，再加起来。
(define (install-polynomial-package)
  (define (negate-termlist termlist)
    (cond ((empty-termlist termlist) termlist)
          (else (adjoin-term
                 (make-term
                  (order (first-terms termlist))
                  (negate (coeff (first-terms termlist))))
                 (negate-termlist (rest-terms termlist))))))

  (define (negate-poly p)
    (make-poly
     (variable p)
     (negate-termlist (term-list p))))

  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))


  ;; 减法就是第一个加上第二个的求负
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  )


;; 2.89
(define (adjoin-term term term-list)
  (cond
   ((=zero? (coeff term)) term-list)
   ((= (order term) (+ (length term-list) 1))
    (cons (coeff term) term-list))
   ((> (order term) (+ (length term-list) 1))
    (cons (coeff term)
          (adjoin-term term (cons 0 term-list))))
   (else (error "error"))))

(define (first-terms term-list)
  (make-term (- (length term-list) 1)
             (car term-list)))


;; 2.90
;; 类似直角坐标和极坐标的方式。首先需要先定义出两种项表的表示。
;; 这里不打算将term封装成一个类型，因为两种项表都依赖同一种类型，它本身已经是通用了。

;; 稀疏型
(define (install-term-list-sparse-package)
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())

  (define (first-terms term-list)
    (car term-list))

  (define (rest-terms term-list)
    (cdr term-list))

  (define (empty-termlist term-list)
    (null? term-list))

  (define (tag x) (attach-tag 'term-list-sparse x))

  (put 'adjoin-term 'term-list-sparse
       (lambda (t tl) (tag (adjoin-term t tl))))

  (put 'the-empty-termlist '()
       (lambda () (tag (the-empty-termlist))))

  (put 'first-terms '(term-list-sparse)
       (lambda (tl) (first-terms tl)))

  (put 'rest-terms '(term-list-sparse)
       (lambda (tl) (tag (rest-terms tl))))

  (put 'empty-termlist '(term-list-sparse)
       (lambda (tl) (empty-termlist tl))))


;; 稠密型
(define (install-term-list-dense-package)
  (define (adjoin-term term term-list)
    (cond
     ((=zero? (coeff term)) term-list)
     ((= (order term) (+ (length term-list) 1))
      (cons (coeff term) term-list))
     ((> (order term) (+ (length term-list) 1))
      (cons (coeff term)
            (adjoin-term term (cons 0 term-list))))
     (else (error "error"))))

  (define (first-terms term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))

  (define (the-empty-termlist) '())

  (define (rest-terms term-list)
    (cdr term-list))

  (define (empty-termlist term-list)
    (null? term-list))

  (define (tag x) (attach-tag 'term-list-dense x))

  (put 'adjoin-term 'term-list-dense
       (lambda (t tl) (tag (adjoin-term t tl))))

  (put 'the-empty-termlist 'term-list-dense
       (lambda () (tag (the-empty-termlist))))

  (put 'first-terms '(term-list-dense)
       (lambda (tl) (first-terms tl)))

  (put 'rest-terms '(term-list-dense)
       (lambda (tl) (tag (rest-terms tl))))

  (put 'empty-termlist '(term-list-dense)
       (lambda (tl) (empty-termlist tl))))


;; 以下是通用型函数
(define (adjoin-term term term-list)
  (let ((type (type-tag term-list)))
    (let ((proc (get 'adjoin-term type)))
      (proc term term-list))))

(define (first-terms term-list)
  (apply-generic 'first-terms term-list))

(define (rest-terms term-list)
  (apply-generic 'rest-terms term-list))

(define (empty-termlist term-list)
  (apply-generic 'empty-termlist term-list))


;; 最纠结的是the-empty-termlist，它有可能是不一样表示的。但是它不需要参数，所以不知道到底应该选择什么表示。这意味着必须修改上层代码。
;; 如果保持无参数，那么就必须写两个构造函数，取消the-empty-termlist，那么上层就必须知道类型了。如果不保存无参数，那就传入一个termlist，就能知道该调用那个过程了。
;; 或许还有别的办法吧。TODO
;; 另一个方案是the-empty-termlist不写到对应的包中。



;; 2.91至2.97
;; TODO
;; 没动力去做这些题，心思都在新的章节了。而且做出来也不能验证，挺糟心的。
;; 就留到下次在做吧。必须记住自己留下的坑。

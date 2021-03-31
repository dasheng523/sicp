(load "tools.ss")

(define (compose f g)
  (lambda args (f (apply g args))))

;;((compose (lambda (x) (list 'foo x)) (lambda (x) (list 'bar x))) 'z)

(define (identity x) x)

(define (compose f g)
  (define (the-composition . args)
    (f (apply g args)))
  the-composition)

(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)


(define (spread-combine h f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (h (apply f (list-head args n))
           (apply g (list-tail args n))))
      (restrict-arity the-combination t))))

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



(define (spread-apply f g)
  (let ((n (get-arity f))
        (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (values (apply f (list-head args n))
                (apply g (list-tail args n))))
      (restrict-arity the-combination t))))


(define (spread-combine h f g)
  (compose h (spread-apply f g)))

(define (compose f g)
  (define (the-composition . args)
    (call-with-values
        (lambda () (apply g args)) f))
  (restrict-arity the-composition (get-arity g)))

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

(define exact-nonnegative-integer?
  (lambda (i)
    (and (positive? i) (exact? i))))

(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (lambda (f)
    (let ((m (+ (get-arity f) 1)))
      (define (the-combination . args)
        (assert (= (length args) m))
        (apply f (list-remove args i)))
      (assert (< i m))
      (restrict-arity the-combination m))))

(define (list-remove lst index)
  (let lp ((lst lst) (index index))
    (if (= index 0)
        (cdr lst)
        (cons (car lst) (lp (cdr lst) (- index 1))))))

#;(((discard-argument 2)
  (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd)

(define (curry-argument i)
  (lambda args
    (lambda (f)
      (assert (= (length args) (- (get-arity f) 1)))
      (lambda (x)
        (apply f (list-insert args i x))))))


(define (list-insert lst index value)
  (let lp ((lst lst) (index index))
    (if (= index 0)
        (cons value lst)
        (cons (car lst) (lp (cdr lst) (- index 1))))))


#;((((curry-argument 2) 'a 'b 'c) (lambda (x y z w) (list 'foo x y z w))) 'd)


(define (permute-arguments . permspec)
  (let ((permute (make-permutation permspec)))
    (lambda (f)
      (define (the-combination . args)
        (apply f (permute args)))
      (let ((n (get-arity f)))
        (assert (= n (length permspec)))
        (restrict-arity the-combination n)))))

(define (make-permutation permspec)
  (define (the-permuter lst)
    (map (lambda (p) (list-ref lst p)) permspec))
  the-permuter)


#;(((permute-arguments 1 2 0 3) (lambda (x y z w) (list 'foo x y z w))) 'a 'b 'c 'd)




(define (r:dot) ".")
(define (r:bol) "∧")
(define (r:eol) "$")

(define (r:seq . exprs)
  (string-append "\\(" (apply string-append exprs) "\\)"))

(define (r:quote string)
  (r:seq (list->string
          (append-map (lambda (char)
                        (if (memv char chars-needing-quoting)
                            (list #\\ char)
                            (list char))
                        )
                      (string->list string)))))
(define chars-needing-quoting '(#\. #\[ #\\ #\∧ #\$ #\*))

;;(r:quote "a.a")


(define (r:alt . exprs)
  (if (pair? exprs)
      (apply r:seq (cons (car exprs)
                         (append-map
                          (lambda (expr)
                            (list "\\|" expr))
                          (cdr exprs))))
      (r:seq)))

;;(r:alt (r:quote "foo") (r:quote "bar") (r:quote "baz"))



(define (r:repeat min max expr)
  (apply r:seq
         (append (make-list min expr)
                 (cond ((not max) (list expr "*"))
                       ((= max min) '())
                       (else (make-list (- max min) (r:alt expr "")))))))

;;(r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))


(define (r:char-from string)
  (case (string-length string)
    ((0) (r:seq))
    ((1) (r:quote string))
    (else (bracket string
                   (lambda (members)
                     (if (lset= eqv? '(#\- #\∧) members)
                         '(#\- #\∧)
                         (quote-bracketed-contents members)))))))

;;(r:char-from "aaaa")


(define (r:char-not-from string)
  (bracket string
           (lambda (members)
             (cons #\∧ (quote-bracketed-contents members)))))
;;(r:char-not-from "aaa")


;; 方括号
(define (bracket string procedure)
  (list->string (append '(#\[)
                        (procedure (string->list string)) '(#\]))))

(define (quote-bracketed-contents members)
  (define (optional char)
    (if (memv char members) (list char) '()))
  (append (optional #\])
          (remove (lambda (c)
                    (memv c chars-needing-quoting-in-brackets))
                  members)
          (optional #\∧)
          (optional #\-)))
(define chars-needing-quoting-in-brackets '(#\] #\∧ #\-))



(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))

(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep -e " (bourne-shell-quote-string expr) " " filename "\n"))

(define (bourne-shell-quote-string string)
  (list->string (append (list #\')
                        (append-map (lambda (char)
                                      (if (char=? char #\')
                                          (list #\' #\\ char #\')
                                          (list char)))
                                    (string->list string)) (list #\'))))


#;(write-bourne-shell-grep-command
 (r:seq (r:quote "umi") (r:quote " ") (r:quote "build"))
 "package.json")

#;(write-bourne-shell-grep-command
 (r:alt (r:quote "umi") (r:quote "build"))
 "package.json")






;;;; 2.3 Wrappers
;; 临时心得
;; 当需要扩展的时候，我一般在某个函数上多加一个参数，然后修改其内部实现。是的这样看来会非常快。但这会隐藏程序背后的逻辑。
;; 应该换成什么样的思维呢？我想要的目的是把背后的逻辑抽离出来。但一般来说背后的逻辑不会非常清晰。应怎么做呢？
;; 背后的逻辑就是不变的逻辑。就拿这个程序，不管单位如何变化，不变的是gas-law-volume的逻辑。
;; 找到背后的逻辑，也就是找到不变的逻辑。
;; 找到之后，就封装变化的逻辑了。变化的逻辑就可借助于领域语言。

;; 关注点分离就是将某段逻辑分成不同的部分。一般来说就是把主逻辑和从逻辑分开来。
(define (gas-law-volume pressure temperature amount)
  (/ (* amount gas-constant temperature) pressure))

(define gas-constant 8.3144621)

(define (sphere-radius volume)
  (expt (/ volume (* 4/3 pi)) 1/3))

(define pi (* 4 (atan 1 1)))

(define (make-unit-conversion f1 f2)
  (assert (> (get-arity f1) 0))
  (assert (> (get-arity f2) 0))
  (restrict-arity (case-lambda
                    [() (cons f1 f2)]
                    [args (apply f1 args)])
                  (get-arity f1)))

(define (unit:* u1 u2)
  (make-unit-conversion
   (compose u2 u1)
   (compose (unit:invert u1) (unit:invert u2))))

(define (units:* . units)
  (let ([n (length units)])
    (cond
     [(= n 1) (car units)]
     [(= n 2) (unit:* (car units) (cadr units))]
     [(> n 2) (apply units:* (unit:* (car units) (cadr units))
                     (cddr units))]
     [else (error "入参数量错误")])))


(define (unit:invert u)
  (let ([fs (u)])
    (make-unit-conversion
     (cdr fs)
     (car fs))))

(define (unit:/ u1 u2)
  (unit:* u1 (unit:invert u2)))

(define (unit:expt u n)
  (assert (exact-nonnegative-integer? n))
  (if (= n 1)
      u
      (unit:* u (unit:expt u (- n 1)))))


;; 华氏度转摄氏度
#;(define fahrenheit-to-celsius
  (make-unit-conversion (lambda (f) (* 5/9 (- f 32)))
                        (lambda (c) (+ (* c 9/5) 32))))

#;(fahrenheit-to-celsius 32)
#;((unit:invert fahrenheit-to-celsius) 20)
#;((unit:expt (unit:invert fahrenheit-to-celsius) 2) 20)

;; 摄氏度转绝对温度
#;(define celsius-to-kelvin
  (let ((zero-celsius 273.15)) ;kelvins
    (make-unit-conversion
     (lambda (c) (+ c zero-celsius))
     (lambda (k) (- k zero-celsius)))))

#;((unit:* fahrenheit-to-celsius celsius-to-kelvin) 80)



(define make-specialized-gas-law-volume
  (unit-specializer gas-law-volume
                    '(expt meter 3) ; output (volume)
                    '(/ newton (expt meter 2)) ; pressure
                    'kelvin ; temperature
                    'mole)) ; amount


(define conventional-gas-law-volume
  (make-specialized-gas-law-volume
   '(expt inch 3) ; output (volume)
   '(/ pound (expt inch 2)) ; pressure
   'fahrenheit ; temperature
   'mole)) ; amount


;; (sphere-radius (conventional-gas-law-volume 14.7 68 1))


(define (unit-specializer procedure implicit-output-unit . implicit-input-units)
  (define (specializer specific-output-unit . specific-input-units)
    (let ((output-converter (make-converter implicit-output-unit specific-output-unit))
          (input-converters (map make-converter specific-input-units implicit-input-units)))
      (define (specialized-procedure . arguments)
        (output-converter
         (apply procedure
                (map (lambda (converter argument) (converter argument))
                     input-converters arguments))))
      specialized-procedure))
  specializer)


;; 创建一个单位节点
(define (create-unit-node u)
  (list u))

;; 在一个节点上挂载一个叶子
(define (unit-node-mount node leaf conversion)
  (append node (list (cons leaf conversion))))

;; 获取节点所有叶子和对应转换器
(define (unit-node-leafs node)
  (cdr node))



(define unit-node-table (make-eqv-hashtable))

(define (register-unit-conversion u1 u2 conversion)
  (define (register u1 u2 conversion)
    (let ([node (or (hashtable-ref unit-node-table u1 #f)
                    (create-unit-node u1))])
      (hashtable-set! unit-node-table
                      u1
                      (unit-node-mount u1 u2 conversion))))
  (register u1 u2 conversion)
  (register u2 u1 (unit:invert conversion)))

;; TODO
(define (make-converter u1 u2)
  (cond
   [(and (simple-unit? u1) (simple-unit? u2))
    (let ([conversions (find-unit-path-conversion u1 u2)])
      (if conversions
          (apply units:* conversions)
          (error (list "找不到单位换算路径" u1 u2))))]
   [(and (compound-unit? u1) (compound-unit? u2))
    ]))


;; 华氏度和摄氏度的关系
(register-unit-conversion 'fahrenheit 'celsius
                          (make-unit-conversion (lambda (f) (* 5/9 (- f 32)))
                                                (lambda (c) (+ (* c 9/5) 32))))

;; 摄氏度与绝对度的关系
(register-unit-conversion 'celsius 'kelvin
                          (let ((zero-celsius 273.15)) ;kelvins
                            (make-unit-conversion
                             (lambda (c) (+ c zero-celsius))
                             (lambda (k) (- k zero-celsius)))))


































;;;;; Exercise 2.1: Arity repair
(define (compose f g)
  (assert (= (get-arity f) 1))
  (let [(n (get-arity g))]
    (define (the-composition . args)
      (assert (= (length args) n))
      (f (apply g args)))
    (restrict-arity the-composition n)))

(define (parallel-combine h f g)
  (let [(n1 (get-arity f))
        (n2 (get-arity g))]
    (assert (and (= 2 (get-arity h))
                 (= n1 n2)))
    (let [(the-combination
           (lambda args
             (assert (= (length args) n1))
             (h (apply f args) (apply g args))))]
      (restrict-arity the-composition n1))))


;;;; tests

#;((parallel-combine list
                 (lambda (x y) (list 'foo x y))
                 (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)

#;((parallel-combine list
                 (lambda (x y) (list 'foo x y))
                 (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)

;;;;;;;;



;;;;;; Exercise 2.2: Arity extension

;;; spread-combine 的 h 必须支持传入两个参数
;;; 必须提供一个函数，用来检测一个proc是否支持n个参数
;;; 无论什么情况，f 的参数是固定数量的，因为它拿 args 的前n项作为输入。
;;; 如果 args是固定的，g可以是不定参数，也可以是固定参数。
;;; 如果 args是不定的，g必须是不定参数
;; 既然可以支持不定参数，那么get-arity就不需要依赖hashtable了，也不需要restrict-arity了。
;; 这里的实现比书本上的强大得多。

(define (support-mask-n? mask n)
  (logbit? n mask))

(define (support-arity-n? proc n)
  (support-mask-n? (procedure-arity-mask proc) n))

(define (proc-args-count-list mask size)
  (if (< mask 0)
      '()
      (let find ([i size] [rs '()])
        (cond
         [(< i 0) rs]
         [(support-mask-n? mask i) (find (- i 1) (cons i rs))]
         [else (find (- i 1) rs)]))))



(define (match-plan a-mask b-mask size)
  (let plan [(a-list (proc-args-count-list a-mask size))]
    (if (null? a-list)
        #f
        (let ([item (car a-list)])
          (if (and (> size item) (logbit? (- size item) b-mask))
              item
              (plan (cdr a-list)))))))


(define (spread-combine h f g)
  (assert (support-arity-n? h 2))
  (let [(f-mask (procedure-arity-mask f))]
    (assert (> f-mask 0))
    (lambda args
      (let [(n (match-plan f-mask (procedure-arity-mask g) (length args)))]
        (assert n)
        (h (apply f (list-head args n))
           (apply g (list-tail args n)))))))



#;(support-arity-n? + 6)
#;(support-arity-n? (lambda (x) x) 1)
#;(support-arity-n? (lambda (x) x) 2)

#;(define substring1
  (case-lambda
    [(s) (substring1 s 0 (string-length s))]
    [(s start) (substring1 s start (string-length s))]
    [(s start end) (substring s start end)]))
#;(proc-args-count-list (procedure-arity-mask substring1) 4)

#;((spread-combine (lambda (a b) (cons a b))
                 (lambda (x y) (list 'foo x y))
                 (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)

#;((spread-combine (lambda (a) a)
                 (lambda (x y) (list 'foo x y))
                 (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)

#;((spread-combine list
                 (lambda (x y) (list 'foo x y))
                 list)
 'a 'b 'c 'd 'e)



;;;;;;;;;; Exercise 2.3: A quickie
(define (parallel-apply f g)
  (let ([n (get-arity f)] [m (get-arity g)])
    (define (the-combination . args)
      (let [(t (length args))]
        (assert (and (= t n) (= t m)))
        (let-values [(fv (apply f args))
                     (gv (apply g args))]
          (apply values (append fv gv)))))
    (restrict-arity the-combination n)))

(define (parallel-combine h f g)
  (compose h (parallel-apply f g)))

#;((parallel-combine list
                   (lambda (x y z) (values x y))
                   (lambda (a b c) (values a b c)))
 'a 'b 'c)

#;((parallel-apply
  (lambda (x y z) (values x z))
  (lambda (a b c) (values a b c)))
 'a 'b 'c)



;;;;;;; Exercise 2.4: As compositions?
;; 就是参数问题，f的参数是不定的，所以m的返回值必须是列表
;; 主要是m参数固定的。
;; 最终参数是不定的
;; 要确定the-combination的参数

;; 使f的返回值是多值
(define (to-values f)
  (restrict-arity
   (lambda args
     (assert (= (length args) (get-arity f)))
     (apply values (apply f args)))
   (get-arity f)))

(define (compose-values f m)
  (compose f (to-values m)))


(define (permute-arguments . permspec)
  (((curry-argument 0) (make-permutation permspec)) compose-values))

(((permute-arguments 1 2 0 3)
  (lambda (x y z w) (list 'foo x y z w)))
 (list 'a 'b 'c 'd))


(define (curry-argument i)
  (lambda args
    (lambda (f)
      (compose-values f (lambda (x) (list-insert args i x))))))

#;((((curry-argument 2) 'a 'b 'c) (lambda (x y z w) (list 'foo x y z w))) 'd)


(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (lambda (f)
    (compose-values f (restrict-arity (lambda args (list-remove args i)) (+ 1 (get-arity f))))))

#;(((discard-argument 2) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd)




;;;;;;;;;; Exercise 2.5: Useful combinators
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


;((((curry-arguments 2) 'a 'b 'c) (lambda (x y z w) (list 'foo x y z w))) 'd)
((((curry-arguments 3 1) 'a 'b 'c) (lambda (x y z w s) (list 'foo x y z w s))) 'e 'd)
((((curry-arguments 1 3) 'a 'b 'c) (lambda (x y z w s) (list 'foo x y z w s))) 'd 'e)

(define (list-replace lst n v)
  (assert (> (length lst) n))
  (let ([head (list-head lst n)] [tail (list-tail lst n)])
    (append head (cons v (cdr tail)))))

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


(((discard-arguments 2) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd)
(((discard-arguments 2 3) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd 'u)
(((discard-arguments 3 2) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd 'u)


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

((compose
  (lambda (x y) (list x y))
  (lambda (x) (values 'b x))
  (lambda (x) (list 'a x)))
 'ys)

((compose
  (lambda (x y) (list x y))
  (lambda (x) (values 'b x))
  (lambda () (list 'a)))
 )







;; Exercise 2.6: Adding * and + to regular expressions
(define (r:* expr)
  (r:repeat 0 #f expr))

(define (r:+ expr)
  (r:repeat 1 #f expr))

#;(write-bourne-shell-grep-command
(r:seq
(r:* (r:quote "@types/"))
(r:quote "lodash"))
"package.json")





;;; Exercise 2.7: A bug, one bad joke, two tweaks, and a revelation
Louis 的建议根本跑不动，会死循环的。
Alyssa 的建议是遍历多个，可是当max特别大，min特别小的时候，生成出来的正则表达式将会非常庞大。
Ben 的建议明显非常简洁，生成的正则表达式也非常小，性能应该也是最好的。


(define (r:repeat min max expr)
  (string-append expr "\\{" (number->string min) "," (or (and max (number->string max)) "")  "\\}"))

(write-bourne-shell-grep-command
 (r:repeat 1 3 (r:quote "1"))
 "package.json")
(write-bourne-shell-grep-command
 (r:repeat 1 #f (r:quote "1"))
 "package.json")


(write-bourne-shell-grep-command
 (r:seq (r:quote "\"") (r:repeat 1 #f (r:dot)) (r:quote "\"") (r:quote ":"))
 "package.json")





;;; Exercise 2.8: Too much nesting
;; 表达式也有两种，一种是需要加括号的，另一种不需要加括号。

;; 无括号表达式
(define (non-parenthese-expr str)
  (list 'non-parenthese-expr str))

(define (non-parenthese-expr? expr)
  (eq? 'non-parenthese-expr (car expr)))

;; 带括号表达式
(define (with-parenthese-expr str)
  (list 'with-parenthese-expr str))

(define (with-parenthese-expr? expr)
  (eq? 'with-parenthese-expr (car expr)))

;; 获取表达式中的正则
(define (get-regular expr)
  (cadr expr))

;; 将表达式翻译成正则
(define (translate-regular expr)
  (cond
   [(non-parenthese-expr? expr) (get-regular expr)]
   [(with-parenthese-expr? expr) (string-append "\\(" (get-regular expr) "\\)")]
   [else (raise "不支持该表达式")]))

;; 右括号的直接返回，没括号的包上括号
(define (sure-parenthese expr)
  (cond
   [(non-parenthese-expr? expr) (with-parenthese-expr (translate-regular expr))]
   [(with-parenthese-expr? expr) expr]
   [else (raise "不支持该表达式")]))


(define chars-needing-quoting '(#\. #\[ #\\ #\∧ #\$ #\*))

(define (r:dot) (non-parenthese-expr "."))
(define (r:bol) (non-parenthese-expr "∧"))
(define (r:eol) (non-parenthese-expr "$"))

(define (r:seq . exprs)
  (non-parenthese-expr
   (apply string-append (map translate-regular exprs))))

(define (r:quote string)
  (non-parenthese-expr
   (list->string
    (append-map (lambda (char)
                  (if (memv char chars-needing-quoting)
                      (list #\\ char)
                      (list char)))
                (string->list string)))))
;;(r:quote "a.a")


(define (r:alt . exprs)
  (if (pair? exprs)
      (with-parenthese-expr
       (apply string-append
              (cons (translate-regular (car exprs))
                    (append-map
                     (lambda (expr)
                       (list "\\|" (translate-regular expr)))
                     (cdr exprs)))))
      (r:seq)))

;;(r:alt (r:quote "foo") (r:quote "bar") (r:quote "baz"))



(define (r:repeat min max expr)
  (non-parenthese-expr
   (string-append (translate-regular expr)
                  "\\{"
                  (number->string min)
                  ","
                  (or (and max (number->string max)) "")
                  "\\}")))


;;(r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))


(define (r:char-from string)
  (case (string-length string)
    ((0) (r:seq))
    ((1) (r:quote string))
    (else (non-parenthese-expr
           (bracket string
                    (lambda (members)
                      (if (lset= eqv? '(#\- #\∧) members)
                          '(#\- #\∧)
                          (quote-bracketed-contents members))))))))

;;(r:char-from "aaaa")


(define (r:char-not-from string)
  (non-parenthese-expr
   (bracket string
            (lambda (members)
              (cons #\∧ (quote-bracketed-contents members))))))
;;(r:char-not-from "aaa")



(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))

(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep -e " (bourne-shell-quote-string (translate-regular expr)) " " filename "\n"))

(define (bourne-shell-quote-string string)
  (list->string (append (list #\')
                        (append-map (lambda (char)
                                      (if (char=? char #\')
                                          (list #\' #\\ char #\')
                                          (list char)))
                                    (string->list string)) (list #\'))))



(write-bourne-shell-grep-command
 (r:seq (r:alt (r:char-not-from "ab") (r:char-from "tls")) (r:repeat 1 5 (r:dot)) (r:quote "\""))
 "package.json")

(write-bourne-shell-grep-command
 (r:seq (r:quote "A") (r:repeat 1 5 (r:quote "P")))
 "package.json")

(write-bourne-shell-grep-command
 (r:seq (r:repeat 2 5 (r:alt (r:quote "ab") (r:quote "aba"))))
 "t.json")

(write-bourne-shell-grep-command
 (r:seq (r:quote "a") (r:repeat 1 5 (r:quote "ab")))
 "t.json")

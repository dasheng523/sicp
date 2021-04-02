(library (common combinator)
  (export assert-msg
          identity
          wrap-infinite-params
          compose
          to-values
          compose-values
          parallel-combine
          parallel-apply
          spread-combine
          spread-apply
          default-argument
          default-arguments
          curry-argument
          curry-arguments
          discard-argument
          discard-arguments
          permute-arguments
          until-true
          eachcall
          nestedcall
          if-condition
          restrict-arity
          )
  (import (chezscheme))



  ;; 如果expression是#f，则抛出异常，否则返回expression的值
  ;; (assert-msg (= 0 0) "错误信息")
  ;; (assert-msg (= 0 1) 'who "错误信息")
  (define-syntax assert-msg
    (syntax-rules ()
      [(_ expression who msg)
       (or expression
           (error who msg))]
      [(_ expression msg)
       (assert-msg expression #f msg)]))

  ;; 生成n个参数的lambda
  ;; (make-lambda-arity (lambda args (apply + args)) 2)
  (define-syntax make-lambda-arity
    (lambda (x)
      (define (repeat expr n)
        (cond
         [(= n 0) '()]
         [(= n 1) (list expr)]
         [else (cons expr (repeat expr (- n 1)))]))
      (define (gen-name n k)
        (map (lambda (_) (datum->syntax k (string->symbol (gensym->unique-string (gensym))))) (repeat 1 n)))
      (syntax-case x ()
        [(k proc n)
         (with-syntax ([(p ...) (gen-name (syntax->datum #'n) #'k)])
           #'(lambda (p ...)
               (proc p ...)))])))


  (define (identity x) x)




  ;; 参数表
  (define arity-table (make-eqv-hashtable))

  ;; 注册函数参数
  (define (restrict-arity-mask proc mask)
    (hashtable-set! arity-table proc mask)
    proc)


  ;; 检查f是否支持n个参数
  ;; (support-arity-n? (lambda (x) x) 2)
  ;; (support-arity-n? (lambda x x) 2)
  (define (support-arity-n? f n)
    (let ([mask (proc-arity-mask f)])
      (logbit? n mask)))

  ;; 获取f的参数mask
  (define (proc-arity-mask f)
    (or (hashtable-ref arity-table f #f)
        (procedure-arity-mask f)))

  ;; 获取mask中最小的位数
  (define (mask-arity-min mask)
    (let find-num [(i 0)]
      (if (logbit? i mask)
          i
          (find-num (+ i 1)))))

  ;; 获取f所支持最小的参数数量
  (define (get-arity f)
    (mask-arity-min (proc-arity-mask f)))

  (define (restrict-arity f n)
    (restrict-arity-mask f (ash 1 n)))




  ;; 将2个参数的函数扩展为支持无限参数的函数(left)
  ;; ((wrap-infinite-params (lambda (a b) (+ a b))) 1 2 3 4)
  (define (wrap-infinite-params f)
    (assert-msg (support-arity-n? f 2) 'wrap-infinite-params "f 必须支持两个入参")
    (lambda (p . ps)
      (fold-left f p ps)))



  ;; 不定参数的compose
  (define compose
    (wrap-infinite-params
     (case-lambda
       [(f) f]
       [(g f)
        (restrict-arity-mask
         (lambda args
           (assert-msg (support-arity-n? f (length args))
                       'compose "参数数量不支持")
           (call-with-values
               (lambda () (apply f args)) g))
         (proc-arity-mask f))])))



  ;; 使f的返回值是列表，则返回values，否则直接返回
  ;; ((to-values (lambda (x) (list x 1 2))) 'a)
  (define (to-values f)
    (compose
     (lambda (x)
       (if (pair? x)
           (apply values x)
           x))
     f))


  ;; f 返回 list ,g 以list里的内容作为参数。
  ;; ((compose-values (lambda (x y z) (+ x y z)) list) 1 2 5)
  (define compose-values
    (wrap-infinite-params
     (case-lambda
       [(f) f]
       [(g f)
        (compose g (to-values f))])))








  (define (parallel-combine h f g)
    (compose h (parallel-apply f g)))

  (define (parallel-apply f g)
    (define (the-combination . args)
      (let [(n (length args))]
        (assert-msg (support-arity-n? f n) "f 参数不支持")
        (assert-msg (support-arity-n? g n) "g 参数不支持")
        (let-values [(fv (apply f args))
                     (gv (apply g args))]
          (apply values (append fv gv)))))
    (restrict-arity-mask the-combination
                         (proc-arity-mask f)))
  #;((parallel-combine list
  (lambda (x y z) (values x y))
  (lambda (a b c) (values a b c)))
  'a 'b 'c)




  (define (spread-combine h f g)
    (compose h (spread-apply f g)))


  ;; 分割参数，前n个参数传入给f，剩余的传给g，并把其结果返回
  ;; n 是f所支持的最小参数个数
  ;; ((spread-apply (lambda (x y) (+ x y)) list) 1 2 3 'b 'c)
  ;; ((spread-apply (lambda (x y) (+ x y)) (lambda (x y z) (list x y z))) 1 2 3 'b 'c)

  (define (spread-apply f g)
    (let* ([f-mask (proc-arity-mask f)]
           [g-mask (proc-arity-mask g)]
           [n (mask-arity-min f-mask)]
           [mask (ash g-mask n)])
      (define (the-combination . args)
        (assert-msg (logbit? (length args) mask) "参数数量错误")
        (let-values ((fv (apply f (list-head args n)))
                     (gv (apply g (list-tail args n))))
          (apply values (append fv gv))))
      (restrict-arity-mask the-combination
                           (ash g-mask n))))



  ;; 调用函数，知道某个返回值不是 #f
  ;; ((until-true (lambda (x) #f) (lambda (x) #f) (lambda (x) 6)) 'x)
  ;; ((until-true (lambda (x) 8) (lambda (x) 6)) 'x)
  ;; ((until-true (lambda (x) 8) (lambda (x) 6)) 'x)
  (define until-true
    (wrap-infinite-params
     (lambda (f g)
       (restrict-arity-mask
        (lambda args
          (or (apply f args)
              (apply g args)))
        (proc-arity-mask f)))))

  ;; 将所有参数都调用f
  (define (eachcall f)
    (lambda args
      (apply values (map f args))))


  ;; 判断条件
  ;; ((if-condition (lambda (a b) (= a 1)) (lambda (a b) (+ a b)) (lambda (a b) (* a b))) 2 3)
  (define (if-condition prod f g)
    (restrict-arity-mask
     (lambda args
       (if (apply prod args)
           (apply f args)
           (apply g args)))
     (proc-arity-mask prod)))

  ;; 嵌套调用，((nestedcall a) 1 2) => ((a 1) 2)
  ;; ((nestedcall (lambda (x) (lambda (y) (lambda (z) (+ x y z))))) 1 5 2)
  (define (nestedcall f)
    (assert-msg (support-arity-n? f 1) 'nestedcall "f 需支持一个参数")
    (lambda args
      (let ([n (length args)])
        (cond
         [(= n 0) f]
         [(= n 1) (f (car args))]
         [else (apply (nestedcall (f (car args)))
                      (cdr args))]))))



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



  ;; 给函数加上默认值
  ;; (((default-argument 0 'ee) (lambda (a b c) (list a b c))) 'bb 'aa)
  (define (default-argument i v)
    (lambda (f)
      (compose-values f
                      (restrict-arity-mask
                       (lambda args
                         (list-insert args i v))
                       (ash (proc-arity-mask f) -1)))))

  #;(let* ([f (lambda (a b c) (list a b c))]
         [g (restrict-arity-mask
             f
             (ash (proc-arity-mask f) -1))])
    (mask-arity-min (proc-arity-mask g)))



  ;; (((default-arguments (cons 2 'a) (cons 0 'b)) (lambda (x y z) (list x y z))) 'c)
  (define (default-arguments spec . rest)
    (let* ([specs (list-sort
                   (lambda (a b) (> (car a) (car b)))
                   (cons spec rest))]
           [n (length specs)]
           [the-first (car specs)])
      (cond
       [(= n 1) (default-argument (car the-first) (cdr the-first))]
       [else (lambda (f)
               ((apply default-arguments (cdr specs))
                ((default-argument (car the-first) (cdr the-first)) f)))])))










  ;; curry参数
  (define (curry-argument i)
    (lambda args
      (lambda (f)
        (compose-values f (lambda (x) (list-insert args i x))))))

  #;((((curry-argument 2) 'a 'b 'c) (lambda (x y z w) (list 'foo x y z w))) 'd)


  ;; curry多个参数
  ;;((((curry-arguments 2) 'a 'b 'c) (lambda (x y z w) (list 'foo x y z w))) 'd)
  ;;((((curry-arguments 3 1) 'a 'b 'c) (lambda (x y z w s) (list 'foo x y z w s))) 'e 'd)
  ;;((((curry-arguments 1 3) 'a 'b 'c) (lambda (x y z w s) (list 'foo x y z w s))) 'd 'e)
  (define (curry-arguments . curryspec)
    (lambda args
      (lambda (f)
        (assert (support-arity-n? f (+ (length args) (length curryspec))))
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
    (restrict-arity-mask the-combination (ash 1 (length curryspec))))





  ;; 省略参数
  (define (discard-argument i)
    (assert (exact-nonnegative-integer? i))
    (lambda (f)
      (compose-values f (restrict-arity (lambda args (list-remove args i)) (+ 1 (get-arity f))))))

  #;(((discard-argument 2) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd)


  ;; 省略参数
  ;;(((discard-arguments 2) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd)
  ;;(((discard-arguments 2 3) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd 'u)
  ;;(((discard-arguments 3 2) (lambda (x y z) (list 'foo x y z))) 'a 'b 'c 'd 'u)
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







  ;; 排列参数
  ;;(((permute-arguments 1 2 0 3) (lambda (x y z w) (list 'foo x y z w))) 'a 'b 'c 'd)
  (define (permute-arguments . permspec)
    (lambda (f)
      (compose-values f (make-permutation permspec))))
  (define (make-permutation permspec)
    (define (the-permuter . lst)
      (map (lambda (p) (list-ref lst p)) permspec))
    (restrict-arity the-permuter (length permspec)))





  )

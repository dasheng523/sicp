(define-syntax let*
  (syntax-rules ()
    [(_ () e1 e2 ...)
     (let ()
       e1 e2 ...)]
    [(_ ([a v1] [b v2] ...) e1 e2 ...)
     (let ([a v1])
       (let* ([b v2] ...)
         e1 e2 ...))]))

(let* ([a 5] [b (+ a a)] [c (+ a b)])
  (list a b c))


(define-syntax mywhen
  (syntax-rules ()
    [(_ test expr1 expr2 ...)
     (if test
         (begin expr1 expr2 ...)
         #f)]))

(let ([x 3])
  (unless (= x 0) (set! x (+ x 1)))
  (mywhen (= x 4) (set! x (* x 2)))
  x)



(((call/cc
   (lambda (k) k))
  (lambda (x) x))
 "HEY!")


(let ([x (call/cc (lambda (k) k))])
  (x (lambda (ignore) "hi")))


(define retry #f)

(define factorial
  (lambda (x)
    (if (= x 0)
        (call/cc (lambda (k) (set! retry k) 1))
        (* x (factorial (- x 1))))))

(factorial 4)
(retry 2)


(call/cc
 (lambda (k)
   (* 5 4)))
;; 这个表达式就是一个很普通的表达式，它仅仅是运行它必须的逻辑。

(call/cc
 (lambda (k)
   k))
;; 这个表达式指的是返回一个 continuation ，当给 continuation 一个值调用它的时候，它就是用这个值重新执行当前上下文的程序。

;; lambda里面的表达式，只不过是原来的需要执行的内容，如果暴露出 continuation ，则意味着在调用函数的那个上下文被引用了。如下：
(define factorial
  (lambda (x)
    (if (= x 0)
        (call/cc (lambda (k) (set! retry k) 1))
        (* x (factorial (- x 1))))))
;; retry关联的 continuation 就是调用(factorial 4)时创建的一个上下文环境，并以这个上下文环境作为lambda的body。
;; 这里的call/cc意思是如果被调用了，那么就保存当前的上下文环境，以便 continuation 可以进一步运行。


(define n 0)

(define print-n
  (lambda (pp)
    (display n)
    (newline)
    (set! n (+ n 1))
    pp))

(define conut
  (lambda (k) k))

((call/cc conut)
 (print-n (call/cc conut)))

;; 到底是如何想出来的呢？


;; 第一个 call/cc 是一个 continuation,它的定义是
(lambda (a1)
  (a1 (print-n (call/cc count))))


;; 第二个call/cc是这样的定义
(lambda (a2)
  ((call/cc conut)
   (print-n a2)))

;; 执行print-n后，会打印一次n，然后返回第二个call/cc定义。
((call/cc count)
 (lambda (a2)
   ((call/cc conut)
    (print-n a2))))

;; 该表达式实际上转化之后

((lambda (a1)
   (a1 (print-n (call/cc count))))
 (lambda (a2)
   ((call/cc conut)
    (print-n a2))))

;; 代换
((lambda (a2)
   ((call/cc conut)
    (print-n a2)))
 (print-n (call/cc count)))

;; 此时执行(print-n (call/cc count))之后， continuation 的定义如下
(lambda (a3)
  ((lambda (a2)
     ((call/cc conut)
      (print-n a2)))
   (print-n a3)))

;; 应用于print-n后，打印一次n。
;; 继续转换
((lambda (a2)
   ((call/cc conut)
    (print-n a2)))
 (lambda (a3)
   ((lambda (a2)
      ((call/cc conut)
       (print-n a2)))
    (print-n a3))))

;; 继续转换
((call/cc conut)
 (print-n (lambda (a3)
            ((lambda (a2)
               ((call/cc conut)
                (print-n a2)))
             (print-n a3)))))

;; 转换
((call/cc conut)
 (print-n (lambda (a3)
            ((call/cc conut)
             (print-n (print-n a3))))))

;; 这样就跟上面的步骤循环了



(system "mkdir ss")

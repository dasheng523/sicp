;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 一些工具函数
;; 将任意scheme对象转换为字符串
(define (->string x)
  (with-output-to-string (lambda () (display x))))

;; 求值一组表达式，返回结果之前，会调用finish。
;; 用于做close，free，或者做结果验证之类操作。
(define-syntax with-finish
  (syntax-rules ()
    [(_ finish ([name e] ...) expr ...)
     (let ([name e] ... )
       (let ([rs (begin expr ...)])
         (finish name) ...
         rs))]
    [(_ finish expr ...)
     (let ([rs (begin expr ...)])
       (finish rs)
       rs)]))

(define (partition-by proc data)
  (let loop ([rs '()] [data data])
    (let-values ([(p rest)
                  (partition
                   (lambda (x)
                     (equal? (proc (car data))
                             (proc x)))
                   data)])
      (if (null? rest)
          (cons p rs)
          (loop (cons p rs)
                rest)))))

(define mapcat
  (lambda (proc ls)
    (apply append (map proc ls))))



(define fail #f)
(define-syntax amb
  (syntax-rules ()
    ((_) (fail))
    ((_ a) a)
    ((_ a b ...)
     (let ((fail0 fail))
       (call/cc
        (lambda (cc)
          (set! fail
            (lambda ()
              (set! fail fail0)
              (cc (amb b ...))))
          (cc a)))))))

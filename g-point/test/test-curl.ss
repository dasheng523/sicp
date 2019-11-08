
(rule (list 'define ?name ?val)
      '(#define ?name ?val))

(rule (make-function-exp ?name ?operands ?retype)
      (list ?retype ?name '( . ?retype '))


(define (c-eval exp env)
  (cond [(c-number? exp) (c->number exp)]
        [(c-variable? exp) (find-in-env exp env)]
        [(c-type? exp) (eval-c-type exp env)]
        [(c-operand? exp) (eval-operand exp env)]
        [(c-common-definition? exp)
         (eval-common-definition exp env)]
        [(c-function-definition? exp)
         (eval-function-definition exp env)]
        [(c-struct-definition? exp)
         (eval-struct-definition exp env)]
        [else (error 'eval "无效表达式" exp)]))

(define (make-type-exp type)
  `(type ,type))

(define (c-type? exp)
  (eq? (exp-type exp) 'type))

(define (eval-c-type exp env)
  (let ([rs (find-in-env (cadr exp) env)])
    (cond [(or (eq? rs 'function)
               (eq? rs 'struct))
           `(* ,(cadr exp))]
          [else (cadr exp)])))


(define (make-struct-definition name nplist)
  (list 'struct name nplist))

(define (c-struct-definition? exp)
  (eq? (exp-type exp) 'struct))

(define (c-struct-name exp)
  (cadr exp))

(define (c-struct-nplist exp)
  (caddr exp))

(define (eval-struct-definition exp env)
  (extend-struct (c-struct-name exp) env)
  `(define-ftype ,(c-struct-name exp)
     (struct
         ,@(map
            (lambda (e) (c-eval e env))
            (c-struct-nplist exp)))))

#|

 (c-eval (make-struct-definition
         'cccdd
         (list
          (make-operand-exp 'int 'one #f)
          (make-operand-exp 'char 'two #t)))
         init-env)

(c-eval (make-struct-definition
         'hoo
         (list
          (make-operand-exp 'cccdd 'a #f)
          (make-operand-exp 'char 'b #t)))
        init-env)

(c-eval (make-function-exp
         'a-func
         (list
          (make-operand-exp 'cccdd 'one #f)
          (make-operand-exp 'hoo 'two #f))
         'int) init-env)

(extend-type 'char1 init-env)

;; 我总是觉得这种方式有什么不妥，我觉得实现里面还有其他部分。
;; 我觉得eval和apply的定义需要搞清楚。这里的两个函数可能不像书里说的那样。
;; 我定义is-pointer的时候，遇到了麻烦。

;; 变量的求值没有问题，问题在于这不是变量，而是一个类型的名字。
(load "***.h")
(define a-func (c-eval 'funcA))
(c-apply a-func "abc" (init-struct 'one-struct))


(c-eval (make-operand-exp 'int 'one #f) init-env)

(c-eval 'char init-env)
(c-eval '(define cddda int) init-env)

(c-eval (make-function-exp
         'abc
         (list
          (make-operand-exp 'int 'one #f)
          (make-operand-exp 'char 'two #t))
         'int) init-env)

(define-syntax cccc
  (lambda (x)
    (syntax-case x ()
      [(k)
       (map
        (lambda (e) (datum->syntax #'k e))
        (c-eval '(define abd 0x008822) init-env))])))

(cccc)

(c-eval '(define aaa 0x008822) init-env)

(c-eval 'aaa init-env)

(c-eval '0x8822 init-env)

|#

(define (c-function-definition? exp)
  (eq? (exp-type exp) 'function))

(define (eval-function-definition exp env)
  (extend-function (function-name exp) env)
  `(foreign-procedure
     ,(symbol->string (function-name exp))
     ,(map
       (lambda (opd) (cadr (c-eval opd env)))
        (function-operands exp))
     ,(c-eval (make-type-exp (function-return-type exp)) env)))


(define (make-function-exp name operand-types return-type)
  (list 'function name operand-types return-type))

(define (function-name exp)
  (cadr exp))

(define (function-operands exp)
  (caddr exp))

(define (function-return-type exp)
  (cadddr exp))


(define (c-operand? exp)
  (eq? (exp-type exp) 'operand))

(define (eval-operand exp env)
  `(,(operand-name exp)
    ,(if (operand-star? exp)
         `[* ,(c-eval (operand-type exp) env)]
         (c-eval (operand-type exp) env))))


(define (make-operand-exp type name star?)
  (list 'operand type name star?))

(define (operand-type opd)
  (make-type-exp (cadr opd)))

(define (operand-name opd)
  (caddr opd))

(define (operand-star? opd)
  (cadddr opd))



(define init-env
  (let ([env (make-eq-hashtable)])
    (extend-type 'int env)
    (extend-type 'char env)
    env))

(define (extend-type name env)
  (hashtable-set! env name 'common))

(define (extend-function name env)
  (hashtable-set! env name 'function))

(define (extend-struct name env)
  (hashtable-set! env name 'struct))


(define (c-variable? exp)
  (symbol? exp))

(define (find-in-env exp env)
  (let* ([val (hashtable-ref env exp #f)])
    (cond [(eq? val #f)
           (error 'find-in-env "变量未绑定" exp)]
          [else val])))


(define (c-common-definition? exp)
  (eq? (exp-type exp) 'define))

(define (eval-common-definition exp env)
  (let* ([var (exp-variable exp)]
         [val (c-eval (exp-value exp) env)])
    (extend-env! env var val)
    `(define ,var ,val)))

(define (extend-env! env var val)
  (hashtable-set! env var val))

(define (make-common-definition var val)
  (list 'define var val))

(define (exp-variable exp)
  (cadr exp))

(define (exp-value exp)
  (caddr exp))

(define (exp-type exp)
  (car exp))


(define (c-number? w)
  (or (number? w)
      (and (symbol? w)
           (equal? "0x" (substring (symbol->string w) 0 2)))))

(define (c->number w)
  (if (number? w)
      w
      (string->number
       (list->string
        (cons #\#
              (cdr (string->list
                    (symbol->string w))))))))

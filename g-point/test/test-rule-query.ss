#|


(define (question-symbol? e)
  (equal? (substring (symbol->string e) 0 1) "?"))

(define (question-symbol-name e)
  (string->symbol (list->string
                   (cdr (string->list (symbol->string e))))))


(define global-rules '())

(define (define-rule pattern body)
  (define remove-same
    (lambda (g)
      (if (null? g)
          '()
          (if (match-fields (car g) pattern)
              (cdr g)
              (cons (car g) (remove-same (cdr g)))))))
  (set! global-rules
    (cons (rule 'name pattern body)
          (remove-same global-rules)))
  'ok
    ;; TODO rule的定义要改改
  )



(define (rule-query fields)
  (cons 'rule-query fields))

(define (rule-query-pattern query)
  (cdr query))

(define (rule-query? pattern)
  (eq? (car pattern) 'rule-query))

(define match-fields
  (lambda (rule pattern)
    (let loop ([pattern pattern]
               [fields (rule-fields rule)])
      (if (and (null? pattern)
               (null? fields))
          #t
          (let ([pcar (car pattern)]
                [fcar (car fields)])
            (cond [(equal? pcar fcar)
                   (loop (cdr pattern) (cdr fields))]
                  [(and (question-symbol? pcar)
                        (question-symbol? fcar))
                   (loop (cdr pattern) (cdr fields))]
                  [else #f]))))))


(define (rule-query-handler query reader frame)
  (define (match-rule grules pattern)
    (memp
     (lambda (rule) (match-fields rule pattern))
     grules))

  (let ([rs (match-rule global-rules (rule-query-pattern query))])
    (if rs
        (handle-rule2 (car rs) reader frame)
        (error 'rule-query-handler "没有匹配的rule"))))


(define (handle-rule2 rule reader frame)
  (define fecth-fields
    (lambda (fields data)
      (map
       (lambda (e)
         (if (question-symbol? e)
             (let ([ename (question-symbol-name e)])
               (cons ename
                     (fold-left
                      (lambda (rs x)
                        (if (eq? (car x) ename)
                            (append rs (list (cdr x)))
                            rs))
                      '()
                      data)))
             e))
       fields)))
  (append frame
          (list
           (fecth-fields
            (rule-fields rule)
            (parse (rule-pattern rule) reader '())))))


(define-rule
  '(c-function ?ret ?ret-pointer ?fname ?args)
  (compose-pattern
   (make-var 'ret)
   (b-var 'ret-pointer #\*)
   (make-var 'fname)
   #\(
   (join-repeat-pattern
    #\,
    (arg 'args))
   #\)
   ))

(define-rule
  '(arg ?p-type ?p-name ?p-pointer ?a-num)
  (compose-pattern
   (b-var 'const 'const)
   (make-var 'p-type)
   (b-var 'p-pointer #\*)
   (make-var 'p-name)
   (or-more
    (compose-pattern #\[ (make-var 'a-num) #\]))
   ))

(parse (rule-query '(c-function ?ret ?ret-pointer ?fname ?args))
       (make-reader
        (open-input-string
         "int test(char a, int *b)"))
       '())
|#

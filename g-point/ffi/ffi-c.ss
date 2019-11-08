(library (ffi ffi-c)
  (import (chezscheme)
          (lib common))

  (define-syntax define-ffi-by-c
    (lambda (x)
      (syntax-case x ()
        [(_ c-expr)
         (let ([ffunc (make-foreign-func
                       (make-cfunc #'c-expr))])
           #`(define #,(ffunc-foreign-name ffunc)
               #,(make-foreign-procedure ffunc)))])))




  (define (make-foreign-procedure ffunc)
    (foreign-procedure
     (ffunc-foreign-name ffunc)
     (ffunc-foreign-params ffunc)
     (ffunc-foreign-return ffunc)))



  (define (make-foreign-func cfunc)
    cfunc)

  (define (ffunc-foreign-name ffunc)
    (cfunc-name ffunc))

  (define (ffunc-foreign-params ffunc)
    (let ([params (cfunc-params ffunc)])
      (map
       (lambda (item)
         (let ([type (cparam-type item)])
           (if (ctype-pointer? type)
               (list #'* (ctype-type type))
               (ctype-type type))))
       params)))

  (define (ffunc-foreign-return ffunc)
    (let ([ret (cfunc-return ffunc)])
      (if (ctype-pointer? ret)
          (list #'* (ctype-type ret))
          (ctype-type ret))))



  (define-ffi-by-c
    "MYSQL_FIELD *STDCALL mysql_fetch_fields (MYSQL_RES *res)")


  (define (format-expr expr-str)
    (define (blank-* strls)
      (cond ((null? strls) '())
            ((null? (cdr strls)) strls)
            ((eq? (car strls) #\,) (blank-* (cdr strls)))
            ((and (eq? (car strls) #\*)
                  (eq? (cadr strls) #\space))
             (cons #\space (cons #\* (blank-* (cdr strls)))))
            ((and (eq? (car strls) #\*)
                  (not (eq? (cadr strls) #\space)))
             (cons #\* (cons #\space (blank-* (cdr strls)))))
            (else
             (cons (car strls) (blank-* (cdr strls))))))
    (list->string (blank-* (string->list expr-str))))



  (define (make-cfunc expr-str)
    (define (convert-datum str)
      (let ([port (open-string-input-port str)])
        (let f ([x (read port)])
          (if (eof-object? x)
              '()
              (cons x
                    (f (read port)))))))
    (define (in-list? item ls)
      (cond ((null? ls) #f)
            ((eq? item (car ls)) #t)
            (else (in-list? item (cdr ls)))))
    (let* ([exprls (convert-datum (format-expr expr-str))]
           [next (lambda () (begin
                              (if (null? (cdr exprls)) (raise "语法错误"))
                              (set! exprls (cdr exprls))))]
           [ret-poiter? #f]
           [func-qualifier '()]
           [func-name '()]
           [params '()])

      (define ret-type (car exprls))
      (next)

      (if (or (eq? (car exprls) '*)
              (eq? (car exprls) '()))
          (begin
            (set! ret-poiter? #t)
            (next)))


      (if (in-list? (car exprls) '(STDCALL))
          (begin
            (set! func-qualifier (car exprls))
            (next)))

      (set! func-name (car exprls))
      (next)

      (set! params (car exprls))

      (list (make-ctype ret-type ret-poiter?)
            func-qualifier func-name params)))

  (make-cfunc "MYSQL_FIELD *STDCALL mysql_fetch_fields (MYSQL_RES *res, int[] flag)")

  (read (open-string-input-port "MYSQL_FIELD *STDCALL mysql_fetch_fields (MYSQL_RES *res)"))

  (define (cfunc-return c-function)
    (car c-function))

  (define (cfunc-qualifier c-function)
    (cadr c-function))

  (define (cfunc-name c-function)
    (caddr c-function))

  (define (cfunc-params c-function)
    (let [params (cadddr c-function)]
      (let iter ([params (car params)])
        (let ([type '()]
              [pointer? #f]
              [name '()])
          (set! type (car params))
          (if (in_array (cadr params) '(* []))
              (set! type (cadr params)))))))

  (define (make-cparam type name)
    (cons type name))

  (define (cparam-type cparam)
    (car cparam))

  (define (cparam-name cparam)
    (cdr cparam))

  (define (make-ctype type pointer?)
    (cons type pointer?))

  (define (ctype-pointer? ctype)
    (car ctype))

  (define (ctype-type ctype)
    (cdr ctype))

  )


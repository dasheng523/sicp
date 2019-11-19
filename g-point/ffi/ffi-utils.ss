(library
    (ffi ffi-utils)
  (export define-enumeration* define-function define-enum-ftype
          define-ffi-from-cform define-ffi-from-cforms
          machine-lib-suffix
          with-pointer
          c-bin-dire
          load-lib
          char*->u8 char*->string)
  (import (chezscheme))


  (define c-bin-dire (string-append (caar (library-directories)) "/c"))

  ;; Uses make-enumeration to define an enum with the following:
  ;; function (name x) -> index
  ;; function (name-ref index) -> symbol
  ;; variable name-enum  -> #>enum-set>
  ;; name-t -> ftype int
  ;; usage: (define-enumeration* NAME (tag1 tag2 tag3 ...))
  (define-syntax define-enumeration*
    (lambda (x)
      (define gen-id
        (lambda (template-id . args)
          (datum->syntax
           template-id
           (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x) x
                              (symbol->string (syntax->datum x))))
                        args))))))
      (syntax-case x ()
        [(_ name (l ...))
         (with-syntax ([base-name (gen-id #'name "" #'name)]
                       [enum-name (gen-id #'name #'name "-enum")]
                       [ref-name (gen-id #'name #'name "-ref")]
                       [name-ftype (gen-id #'name #'name "-t")])
           (indirect-export base-name enum-name ref-name name-ftype)
           #'(begin
               (define enum-name (make-enumeration '(l ...)))
               (define base-name
                 (lambda (x)
                   (let ([r ((enum-set-indexer enum-name) x)])
                     (if r r
                         (assertion-violation 'enum-name
                                              "symbol not found"
                                              x)))))
               (define ref-name
                 (lambda (index)
                   (list-ref (enum-set->list enum-name) index)))
               (define-ftype name-ftype int)))])))


  (define-syntax define-function
    (lambda (x)
      (syntax-case x ()
        [(_ name (args ...) ret)
         #'(define name
             (foreign-procedure (symbol->string 'name) (args ...) ret))]
        [(_ name entry (args ...) ret)
         #'(define name
             (foreign-procedure 'entry (args ...) ret))])))


  (define-syntax define-enum-ftype
    (lambda (x)
      (define map-with-index
        (lambda (p ls)
          (let iter ([i 0] [p p] [ls ls])
            (cond ((null? ls) '())
                  (else (cons (p (car ls) i)
                              (iter (+ i 1) p (cdr ls))))))))
      (syntax-case x ()
        [(_ fname ([ename val] ...) ftype)
         #'(begin
             (define ename val) ...
             (define-ftype fname ftype)
             (export ename ...))]
        [(_ fname (ename ...) ftype)
         (let* ([subenames
                 (map-with-index
                  (lambda (name index)
                    `(,name ,index))
                  #'(ename ...))])
           #`(define-enum-ftype fname
               [#,@subenames]
               ftype))])))

  ;; TODO 需要考虑异常情况
  (define-syntax with-pointer
    (lambda (x)
      (syntax-case x ()
        [(_ ([b v] ...) e ... en)
         #'(let ([b v] ...)
             e ...
             (let ([rs en])
               (foreign-free (ftype-pointer-address b)) ...
               rs))])))



  (define (machine-lib-suffix)
    (begin
      (case (machine-type)
        [(i3le ti3le a6le ta6le) ".so.6"]
        [(i3osx ti3osx a6osx ta6osx) ".dylib"]
        [else ".so"])))

  (define (load-lib name)
    (load-shared-object (string-append c-bin-dire "/" name (machine-lib-suffix))))



  (define-syntax define-ffi-from-cform
    (lambda (x)
      (define (syntax->string s)
        (symbol->string (syntax->datum s)))

      (define (parse-rettype ret fname)
        (let ([namstr (syntax->string fname)]
              [retstr (syntax->string ret)])
          (if (eq? (string-ref namstr 0) #\*)
              (list #'* ret)
              ret)))

      (define (parse-fname namstr)
        (if (eq? (string-ref namstr 0) #\*)
            (substring namstr 1 (string-length namstr))
            namstr))

      (define (parse-params pls)
        (values
         (let iter ([ls pls])
           (if (or (null? ls)
                   (eq? (syntax->datum (car ls)) 'void))
               '()
               (let ([type (symbol->string (syntax->datum (car ls)))]
                     [param (symbol->string (syntax->datum (cadr ls)))])
                 (if (eq? (string-ref type 0) #\,)
                     (set! type (substring
                                 type 1 (string-length type))))
                 (cons (if (eq? (string-ref param 0) #\*)
                           (list #'* (car ls))
                           (car ls))
                       (iter (cddr ls))))))))
      (syntax-case x ()
        [(_ ret fname (param ...))
         #`(define #,(datum->syntax #'fname
                                    (string->symbol (parse-fname (syntax->string #'fname))))
             (foreign-procedure
              #,(parse-fname (syntax->string #'fname))
              (#,@(parse-params #'(param ...)))
              #,(parse-rettype #'ret #'fname)))])))


  (define-syntax define-ffi-from-cforms
    (syntax-rules ()
      [(_ (ret fname (param ...)) ...)
       (begin
         (define-ffi-from-cform ret fname (param ...))
         ...)]))


  (define (char*->string address)
    (utf8->string
     (apply bytevector
            (let iter ([i 0])
              (let ((bit (foreign-ref 'unsigned-8 address i)))
                (if (= bit 0)
                    '()
                    (cons bit (iter (+ i 1)))))))))

  (define (char*->u8 fp len)
    (let ([bt (make-bytevector len)]
          [addr (ftype-pointer-address fp)])
      (let loop ([i 0])
        (if (= i len)
            bt
            (begin
              (bytevector-u8-set!
               bt i
               (foreign-ref 'unsigned-8 addr i))
              (loop (+ i 1)))))))


  )

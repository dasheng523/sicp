(library (webserver utils)
  (export record-method)
  (import (chezscheme))

  (define-syntax record-method
    (lambda (x)
      (syntax-case x ()
        [(_ name)
         #'(define name
             (lambda (record) (record '(name))))]
        [(k name params)
         #`(define name
             (lambda (record #,@#'params)
               (record (list 'name #,@#'params))))])))
  )

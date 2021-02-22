(define env
  '((lib-dire . "e:/project/sicp/g-point")
    (lib-ext . ".ss")))

(define (init-system)
  (begin
    (library-directories (cdr (assq 'lib-dire env)))
    #;(library-extensions (cdr (assq 'lib-ext env)))
    (compile-imported-libraries #f)
    (library-requirements-options invoke)
    #t))


(init-system)


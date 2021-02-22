(library (lib app)
  (export init-system)
  (import (chezscheme))


  (define (init-system env)
    (begin
      (library-directories (get-hash-table env 'lib-dire (current-directory)))
      (library-extensions (get-hash-table env 'lib-ext ".so"))
      (compile-imported-libraries #f)
      (library-requirements-options invoke)
      #t))
  )

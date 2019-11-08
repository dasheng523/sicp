(library (testload)
  (export init load?
          ;make-curl
          )
  (import (chezscheme)
          )

  (define (init)
    (unless (foreign-entry? "getenv")
      (load-shared-object "libc.dylib")))


  (define load?
    (begin (init)
           (foreign-entry? "getenv")))

  #;(define make-curl
    (begin
      (init)
      (lambda ()
        (curl-easy-init)))
    )
)

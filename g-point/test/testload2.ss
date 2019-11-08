(library (testload)
  (export getenv1)
  (import (chezscheme))

  (define init
    (begin (load-shared-object "libc.dylib")))


  (define getenv1
    (foreign-procedure "getenv" (string) string))

  )

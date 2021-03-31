(library (common file)
  (export
   read-string-with-file
   )
  (import (chezscheme)
          (lib common)
          (common combinator))


  (define (read-string-with-file path procedure)
    (call-with-input-file path (compose procedure get-string-all)))

  #;(read-string-with-file "e:/aaa.txt" display)


  )

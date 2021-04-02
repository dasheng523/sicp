(library (common file)
  (export
   read-string-with-file
   )
  (import (chezscheme)
          (lib common)
          (common combinator))

  ;; 读取文件
  (define (read-string-with-file path procedure)
    (call-with-input-file path (compose procedure get-string-all)))

  #;(read-string-with-file "e:/aaa.txt" display)


  ;; 写入文件，文件不存在也可以写
  (define (write-string-file path string)
    (call-with-output-file path
      (lambda (p)
        (put-string p string))))

  #;(write-string-file "e:/aaa1.txt" "888")

  )

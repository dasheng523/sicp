(library (lib regex)
  (export
   reg-find
   reg-replace)
  (import (chezscheme)
          (ffi ffi-utils)
          (ffi regex)
          (lib common))


  (define (string-mutisub str indexs)
    (if (null? indexs)
        '()
        (cons (substring str (caar indexs) (cdar indexs))
              (string-mutisub str (cdr indexs)))))

  (define (string-replace str start end newstr)
    (let ([pie1 (substring str 0 start)]
          [pie2 (substring str end (string-length str))])
      (string-append pie1 newstr pie2)))



  (define (reg-find regex str)
    (with-pointer
     ([compreg-p (init-regex-t)])
     (let ([status (regcomp compreg-p regex REG_EXTENDED)])
       (if (= status REG_NOERROR)
           (with-pointer
            ([matchdata-p (init-regmatch-array-t)])
            (let ([status (regexec compreg-p str 100 matchdata-p 0)])
              (regfree compreg-p)
              (if (= REG_NOMATCH status)
                  ""
                  (string-mutisub str
                                  (regmatch-array->list matchdata-p)))))
           (begin
             (regfree compreg-p)
             (raise
              (condition
               (make-error)
               (make-message-condition "正则语法错误"))))))))


  (define (reg-replace regex target newstr)
    (with-pointer
     ([compreg-p (init-regex-t)])
     (let ([status (regcomp compreg-p regex REG_EXTENDED)])
       (if (= status REG_NOERROR)
           (with-pointer
            ([matchdata-p (init-regmatch-array-t)])
            (regexec compreg-p target 100 matchdata-p 0)
            (regfree compreg-p)
            (let ([mls (regmatch-array->list matchdata-p)])
              (string-replace target
                              (caar mls)
                              (cdar mls)
                              newstr)))
           (begin
             (regfree compreg-p)
             (raise
              (condition
               (make-error)
               (make-message-condition "正则语法错误"))))))))


  )


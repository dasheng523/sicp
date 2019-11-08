(library (lib common)
  (export string-split string-empty? string-empty? string-join
          ->string
          partial ->>
          mapcat
          file-size println)
  (import (chezscheme))

  ;; string

  ;; 将 aa-bb-cc 分割为 '(aa bb cc)
  (define (string-split str delim)
    (define in (open-input-string str))
    (let recur ((out (open-output-string)))
      (define c (read-char in))
      (cond ((eof-object? c)
             (list (get-output-string out)))
            ((char=? c delim)
             (cons (get-output-string out)
                   (recur (open-output-string))))
            (else
             (write-char c out)
             (recur out)))))

  ;; 判断一个字符串是不是空串
  (define (string-empty? str)
    (string=? str ""))

  ;; 将'(aa bb cc) 合并成 aa-bb-cc
  (define (string-join strs delim)
    (fold-left (lambda (rs str)
                 (string-append rs
                                (if (string-empty? rs) "" delim)
                                str)) "" strs))

  ;; 将x合理变成字符串
  (define (->string x)
    (with-output-to-string (lambda () (display x))))


  (define mapcat
    (lambda (proc ls)
      (apply append (map proc ls))))


  #;(define (bytevector->string bv)
    (call-with-port
     (open-bytevector-input-port bv (native-transcoder))
     (lambda (p)
       (get-string-all p))))


  ;; file相关函数
  (define (file-size filename)
    (call-with-input-file filename
      (lambda (port)
        (let loop ((c (read-char port))
                   (count 0))
          (if (eof-object? c)
              count
              (loop (read-char port) (+ 1 count)))))))


  ;; 其他函数

  (define-syntax partial
    (syntax-rules ()
      [(_ (fn arg ...))
       (lambda (x)
         (fn arg ... x))]
      [(_ fn)
       (lambda (x)
         (fn x))]))


  (define-syntax ->>
    (lambda (x)
      (syntax-case x ()
        [(_ e) #'e]
        [(_ e1 e2 ...)
         (let unfold ([ls #'(e2 ...)]
                      [rs #'e1])
           (cond ((null? ls) rs)
                 (else (unfold (cdr ls)
                               #`((partial #,(car ls)) #,rs)))))])))

  (define (println . args)
    (cond [(null? args) (newline)]
          [else (begin
                  (display (car args))
                  (apply println (cdr args)))]))


)

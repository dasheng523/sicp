(library (lib common)
  (export string-split string-empty? string-empty? string-join string-trim-left string-trim-right string-trim
          ->string
          partial ->>
          mapcat
          file-size println
          eval-from-str)
  (import (chezscheme))

  ;; string


  (define string-trim-left
    (lambda (s)
      (list->string
       (let loop ([ls (string->list s)])
         (cond [(null? ls) '()]
               [(char-whitespace? (car ls)) (loop (cdr ls))]
               [else ls])))))

  (define string-trim-right
    (lambda (s)
      (list->string
       (reverse
        (let loop ([ls (reverse (string->list s))])
          (cond [(null? ls) '()]
                [(char-whitespace? (car ls)) (loop (cdr ls))]
                [else ls]))))))

  (define string-trim
    (lambda (s)
      (string-trim-right
       (string-trim-left s))))


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


  (define-syntax eval-from-str
    (lambda (x)
      (define read-str
        (lambda (s k)
          (let ([p (open-input-string s)])
            (let f ([x (read p)])
              (if (eof-object? x)
                  (begin (close-input-port p) '())
                  (cons (datum->syntax k x)
                        (f (read p))))))))
      (syntax-case x ()
        [(k s)
         (let ([fn (datum s)])
           (with-syntax ([(exp ...) (read-str fn #'k)])
             #'(begin exp ...)))])))

)

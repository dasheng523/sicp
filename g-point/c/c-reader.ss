#;(library (c c-reader)
  (export make-reader
          read-word
          peek-word
          copy-reader
          the-input-port
          reset!
          char-separator?
          char-identifier?
          display-current-near-words
          )
  (import (chezscheme)))


;;;;;;;;;;;; reader 定义 ;;;;;;;
;; 创建一个文件阅读器,
(define (make-reader p)
  (let ([position (file-position p)])
    (lambda (x)
      (record-case
       x
       [(read-word) ()
        (let f ([c (peek-char p)])
          (cond
           [(eof-object? c) (read-char p)]
           [(char-whitespace? c)
            (begin
              (read-char p)
              (f (peek-char p)))]
           [(eq? #\" c) (read p)]
           [(char-identifier? c)
            (if (char-numeric? c)
                (read p)
                (string->symbol
                 (list->string
                  (let rut ([rc (peek-char p)])
                    (if (and (not (eof-object? rc))
                             (char-identifier? rc))
                        (cons (read-char p) (rut (peek-char p)))
                        '())))))]
           [(eq? #\/ c)
            (read-char p)
            (cond [(eq? #\/ (peek-char p))
                   (begin (read-char-until p #\newline)
                          (f (peek-char p)))]
                  [(eq? #\* (peek-char p))
                   (begin (read-char-until p #\* #\/)
                          (f (peek-char p)))]
                  [else c])]
           [else (read-char p)]))]
       [(reset!) () (file-position p position)]
       [(the-input-port) () p]))))

;; 读一个词出来
(define (read-word reader)
  (reader '(read-word)))

;; 复制一个reader
(define (copy-reader reader)
  (make-reader (the-input-port reader)))

;; 读一个词，不改变指针
(define (peek-word reader)
  (let ([reader (copy-reader reader)])
    (with-finish
     (lambda (_) (reset! reader))
     (read-word reader))))

;; 获取reader的input-port
(define (the-input-port reader)
  (reader '(the-input-port)))

;; 重置reader的指针
(define (reset! reader)
  (reader '(reset!)))


;; 判断字符是否是分隔符
(define (char-separator? c)
  (or (eq? #\; c) (eq? #\* c) (eq? #\, c) (eq? #\# c)
      (eq? #\( c) (eq? #\) c)
      (eq? #\< c) (eq? #\> c)
      (eq? #\{ c) (eq? #\} c)
      (eq? #\[ c) (eq? #\] c)))

;; 判断字符是否是标识
(define (char-identifier? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (eq? #\_ c)))

;; 读字符一直检查到终止符为止
(define (read-char-until p c . cs)
  (let loop ()
    (cond [(eof-object? (peek-char p)) #t]
          [(eq? c (peek-char p))
           (read-char p)
           (let iter ([c (peek-char p)]
                      [ls cs])
             (cond [(null? ls) #t]
                   [(eq? (car ls) c) (iter (read-char p) (cdr ls))]
                   [else (loop)]))]
          [else (begin (read-char p)
                       (loop))])))

;; 打印当前词语以及附近的词语
(define (display-current-near-words reader)
  (println "当前词: " (read-word reader))
  (newline)
  (println
   (let* ([ip (the-input-port reader)]
          [pos (file-position ip)]
          [result (read-string-by-pos
                   ip
                   (if (< (- pos 20) 0) 0 (- pos 20))
                   (+ pos 20))])
     (file-position ip pos)
     result)))

(define (println . args)
  (cond [(null? args) (newline)]
        [else (begin
                (display (car args))
                (apply println (cdr args)))]))


(define (read-string-by-pos ip start end)
  (file-position ip start)
  (list->string
   (let loop ([start start]
              [end end])
     (cond [(or (>= start end)
                (eof-object? (peek-char ip)))
            '()]
           [else (cons (read-char ip)
                       (loop (+ start 1) end))]))))


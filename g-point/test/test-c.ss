
;; 思路其实很简单了，我已经把c-define的解析做出来了，只要我把环境构造出来，然后把其他的C表达式的模式写出来，基本上完工了。

(define parse
  (lambda (rt reader env)
    ((rt-apply-reset c-define-parser reader) env)))

(define extend-env!
  (lambda (env expr val)
    (cons (cons expr val) env)))


(define c-eval
  (lambda (env expr)
    ))



;; 预处理开始标记
(define preprocessing-beginer
  (rt-fetch-the-char #\#))

;; #define的结构
(define c-define-definition
  (let ([fake-newline (rt-sequence
                       (rt-fetch-the-char #\\)
                       (skip-until rt-space
                                   (rt-fetch-the-char #\newline)))]
        [kuohao-rt (rt-and
                    (rt-fetch-the-char #\()
                    (repeat-until
                     (skip-until (skip-until
                                  rt-space
                                  (rt-fetch-the-char #\,))
                                 rt-word)
                     (rt-fetch-the-char #\))))])
    (rt-sequence
     preprocessing-beginer
     (rt-fetch-the-word 'define)
     (skip-until
      rt-space
      (rt-or
       (rt-sequence rt-identifier kuohao-rt)
       rt-identifier))
     (repeat-until    ;; TODO 这里需要支持没有define的body那种情况。
      (skip-until (rt-or fake-newline rt-space)
                  rt-word)
      (rt-or rt-eof
             (rt-fetch-the-char #\newline))))))

;; #define的解析
(define c-define-parser
  (result-handler
   c-define-definition
   (lambda (rs)
     (lambda (env)
       (let ([name (caddr rs)]
             [val (c-eval (cadddr rs) env)])
         (extend-env! env name val)
         '(c-define ,name ,val))))))


(define c-preprocessing-statement-parser
  (rt-or c-define-parser))


(define c-common-statement-rt
  (rt-and (rt-not (peek preprocessing-beginer))
          (repeat-before (skip-until rt-whitespace rt-word)
                         (rt-or preprocessing-beginer rt-eof))))


(define c-common-statement-parser
  (result-handler
   c-common-statement-rt
   (lambda (rs)
     (lambda (env)
       (let ([reader (list->reader rs)])
         (parse all-c-common-statement reader env))))))


(define all-c-common-statement)
(define list->reader)


































(define inner-keyword?
  (lambda (w)
    (and (symbol? w)
         (not (memq w '(for if while do extern))))))



(define parse
  (lambda (reader env)
    (let loop ([rs '()])
      (let ([word (read-word reader)])
        (cond [(eof-object? word) rs]
              [(eq? word #\#)
               (loop (append rs (preprocessing reader env)))]
              [(inner-keyword? word)
               (loop (append rs (p-eval word env)))]
              [(memq word '(#\; #\,))
               (loop rs)]
              [(memq word '(#\( #\{ #\[))
               (loop (append rs (list (simple-parse reader))))]
              [(memq word '(#\) #\} #\])) rs]
              [else (loop (append rs (list word)))])))))


(define preprocessing-env (make-eq-hashtable))

(define preprocessing
  (lambda (reader env)
    (let [(word (read-word reader))]
      (cond [(eq? 'define word)
             (c-define-preprocessing reader env)]
            [else '()]))))


(define make-comp-exp
  (lambda (params body)
    (cons params body)))

(define params-list
  (lambda (comp-exp)
    (car comp-exp)))

(define body-list
  (lambda (comp-exp)
    (cdr comp-exp)))



(c-define-preprocessing
 (make-reader
  (open-input-string
   " aaa 1"))
 '())


;; 预处理，接受一个reader，返回token列表。其中还将define的内容放入环境中；
(define c-define-preprocessing
  (lambda (reader preprocessing-env)
    (define read-util-space
      (lambda (reader)
        (let ([ip (the-input-port reader)])
          (if (eq? #\space (peek-char ip))
              (read-char ip))
          (list->string
           (let read-info ([c (read-char ip)])
             (cond [(eq? c #\space) '()]
                   [(eof-object? c) '()]
                   [else (cons c (read-info (read-char ip)))]))))))

    (define force-end #t)
    (define read-util-line-end
      (lambda (reader)
        (define line-end?
          (lambda (c)
            (let ([rs (and (eq? c #\newline)
                           force-end)])
              (if (not force-end) #t)
              rs)))

        (let ([ip (the-input-port reader)])
          (list->string
           (let read-info ([c (read-char ip)])
             (cond [(line-end? c) '()]
                   [(and (eq? c #\\) (char-whitespace? (peek-char ip)))
                    (set! force-end #f)
                    (read-info (read-char ip))]
                   [(eof-object? c) '()]
                   [else (cons c (read-info (read-char ip)))]))))))


    (define simple-keys?
      (lambda (keys)
        (= 1 (length keys))))
    (define extend-env!
      (lambda (keys vals)
        (hashtable-set! preprocessing-env
                        (car keys) vals)))

    (let* ([keystr (read-util-space reader)]
           [valstr (read-util-line-end reader)]
           [keys (simple-parse (make-reader (open-input-string keystr)))]
           [vals (p-eval (parse (make-reader (open-input-string valstr))
                                preprocessing-env)
                         preprocessing-env)])
      (if (simple-keys? keys)
          (extend-env! keys vals)
          (extend-env! keys (make-comp-exp (cadr keys)
                                           vals)))
      (list #\# 'define (car keys) vals))))


(define second-handler #f)
(define p-eval
  (lambda (exp env)
    (define (xnumber w)
      (string->number
       (list->string
        (cons #\#
              (cdr (string->list
                    (symbol->string w)))))))
    (define x-number?
      (lambda (w)
        (eq? "0x" (substring (symbol->string w) 0 2))))

    (define simple-symbol?
      (lambda (w)
        (not (comp-symbol? w))))

    (define comp-symbol?
      (lambda (w)
        (procedure? (lookup w))))

    (define lookup
      (lambda (w)
        (hashtable-ref env w #f)))

    (define (bing-val-list comp-exp real-list)
      (define make-map-relate
        (lambda (plist rlist)
          (let ([ht (make-eq-hashtable)])
            (let loop ([plist plist] (rlist rlist))
              (if (null? plist)
                  ht
                  (begin
                    (hashtable-set! ht (car plist) (car rlist))
                    (loop (cdr plist) (cdr rlist))))))))
      (let* ([plist (params-list comp-exp)]
             [blist (body-list comp-exp)]
             [ht (make-map-relate plist real-list)])
        (let exp-replace ([blist blist]
                          [rs '()])
          (cond [(null? blist) rs]
                [(pair? (car blist))
                 (exp-replace
                  (cdr blist)
                  (append rs
                          (list (exp-replace (car blist) '()))))]
                [(memq (car blist) plist)
                 (exp-replace
                  (cdr blist)
                  (append rs
                          (list
                           (hashtable-ref
                            ht
                            (car (memq (car blist) plist)) #f))))]
                [else (exp-replace
                       (cdr blist)
                       (append rs (list (car blist))))]))))

    (define make-second-handler
      (lambda (comp-exp)
        (lambda (exp)
          (set! second-handler #f)
          (bing-val-list comp-exp exp))))

    (cond [second-handler
           (second-handler exp)]
          [(or (number? exp)
               (string? exp)
               (inner-keyword? exp))
           exp]
          [(x-number? exp) (xnumber exp)]
          [(simple-symbol? exp)
           (lookup exp)]
          [(comp-symbol? exp)
           (set! second-handler
             (make-second-handler (lookup exp)))
           '()]
          [(pair? exp)
           (map (lambda (e)
                  (p-eval e env))
                exp)]
          [else (error 'p-eval "该符号未定义")])
    ))


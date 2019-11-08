;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 一些工具函数
;; 将任意scheme对象转换为字符串
(define (->string x)
  (with-output-to-string (lambda () (display x))))

;; 求值一组表达式，返回结果之前，会调用finish。
;; 用于做close，free，或者做结果验证之类操作。
(define-syntax with-finish
  (syntax-rules ()
    [(_ finish ([name e] ...) expr ...)
     (let ([name e] ... )
       (let ([rs (begin expr ...)])
         (finish name) ...
         rs))]
    [(_ finish expr ...)
     (let ([rs (begin expr ...)])
       (finish rs)
       rs)]))

(define (partition-by proc data)
  (let loop ([rs '()] [data data])
    (let-values ([(p rest)
                  (partition
                   (lambda (x)
                     (equal? (proc (car data))
                             (proc x)))
                   data)])
      (if (null? rest)
          (cons p rs)
          (loop (cons p rs)
                rest)))))





(define fail #f)
(define-syntax amb
  (syntax-rules ()
    ((_) (fail))
    ((_ a) a)
    ((_ a b ...)
     (let ((fail0 fail))
       (call/cc
        (lambda (cc)
          (set! fail
            (lambda ()
              (set! fail fail0)
              (cc (amb b ...))))
          (cc a)))))))


(define (parse pattern reader frame)
  (cond [(eof-object? (peek-word reader))
         (error 'parse "eof")]
        [(singleton-pattern? pattern)
         (let* ([reader (copy-reader reader)]
                [word (read-word reader)])
           (if (eq? word pattern)
               frame
               (begin
                 (reset! reader)
                 (error 'parse "token" pattern word))))]
        [(var? pattern)
         (append frame
                 (list (cons (var-name pattern)
                             (read-word reader))))]
        [(b-var? pattern)
         (b-var-handle pattern reader frame)]
        [(compose? pattern)
         (handle-compose pattern reader frame)]
        [(or-more? pattern)
         (let loop ([p (or-more-pattern pattern)]
                    [fm frame])
           (amb
            (loop p
                  (guard (_ [#t (amb)])
                    (parse p reader fm)))
            fm))]
        [(rule? pattern)
         (handle-rule pattern reader frame)]))


(define (handle-join pattern reader frame)
  (define next
    (lambda (rs)
      (guard (_ [#t (amb)])
        (parse (join-sep pattern) reader frame)
        (nnext rs))))
  (guard (_ [#t '()])
    (let ([rs (parse (join-item pattern) reader frame)])
      (amb (list rs)
           (next rs)))))




(define handle-compose
  (lambda (pattern reader frame)
    (let loop ([ps (cdr pattern)]
               [rs '()])
      (if (null? ps)
          rs
          (loop (cdr ps)
                (append rs (parse (car ps) reader '())))))))



(define rule?
  (lambda (pattern)
    (eq? (car pattern) 'rule)))

(define rule
  (lambda (name fields pattern)
    (list 'rule name fields pattern)))

(define rule-name
  (lambda (rule)
    (cadr rule)))

(define rule-fields
  (lambda (rule)
    (caddr rule)))

(define rule-pattern
  (lambda (rule)
    (cadddr rule)))

(define make-rule
  (lambda (fields body)
    (lambda (name)
      (rule name fields body))))



(define (handle-rule rule reader frame)
  (define (aplist . e)
    (apply list e))
  (list (list (rule-name rule)
              (apply map aplist
                     (get-field-datas
                      (rule-fields rule)
                      (parse (rule-pattern rule) reader '()))))))

(define b-var
  (lambda (name token)
    (list 'b-var name token)))

(define b-var?
  (lambda (pattern)
    (eq? 'b-var (car pattern))))

(define b-var-name
  (lambda (pattern)
    (cadr pattern)))

(define b-var-token
  (lambda (pattern)
    (caddr pattern)))

(define b-var-handle
  (lambda (pattern reader frame)
    (let* ([reader (copy-reader reader)]
           [match? (equal? (read-word reader)
                           (b-var-token pattern))])
      (unless match? (reset! reader))
      (append frame
              (list
               (cons (b-var-name pattern)
                     match?))))))

#|

(parse (compose-pattern
        (make-var 'ret)
        (make-var 'name)
        (func-args 'args))
       (make-reader
        (open-input-string
         "int test (char a[2], int b[1])"))
       '())

(define func-args
  (make-rule '(p-type p-name p-pointer a-num)
             (compose-pattern
              #\(
              (join-repeat-pattern
               #\,
               (compose-pattern
                (b-var 'const 'const)
                (make-var 'p-type)
                (b-var 'p-pointer #\*)
                (make-var 'p-name)
                (or-more
                 (compose-pattern #\[ (make-var 'a-num) #\]))
                ))
              #\))))


(parse (compose-pattern
        (make-var 'ret)
        (make-var 'name)
        (func-args 'args))
       (make-reader
        (open-input-string
         "int test (char a, int b)"))
       '())


|#


(define (get-field-datas fields data)
  (define fetch-one-field
    (lambda (field data)
      (map cdr
           (filter
            (lambda (e) (eq? field (car e)))
            data))))
  (define fetch-all-data
    (lambda (fields data)
      (cond [(null? fields) '()]
            [else
             (cons (fetch-one-field (car fields) data)
                   (get-field-datas (cdr fields) data))])))
  (fetch-all-data fields data))


(define (pguess datas)
  (define merge-one
    (lambda (e ls)
      (if (null? ls)
          '()
          (cons
           (cons e (car ls))
           (merge-one e (cdr ls))))))
  (cond [(null? (cdr datas)) (map list (car datas))]
        [else
         (let ([rest (pguess (cdr datas))])
           (let loop ([col (car datas)]
                      [rs '()])
             (if (null? col)
                 rs
                 (loop (cdr col)
                       (append rs
                               (merge-one (car col) rest))))))]))



(define (or-more p)
  (cons 'or-more p))

(define (or-more? p)
  (eq? 'or-more (car p)))

(define (or-more-pattern p)
  (cdr p))


(define (join-repeat-pattern sep pattern)
  (compose-pattern
   pattern
   (or-more (compose-pattern sep pattern))))



(define (singleton-pattern? p)
  (and (not (pair? p))
       (or (symbol? p)
           (char? p))))

(define (compose-pattern x . rest)
  (cons 'compose (cons x rest)))


(define (compose? p)
  (eq? 'compose (car p)))


(define (var? p)
  (eq? '? (car p)))

(define (var-name p)
  (cdr p))

(define (make-var name)
  (cons '? name))

(define (expand-question-mark symbol)
  (let ([chars (symbol->string symbol)])
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))





#|
;;;;;;;;;;;;;;;;;;;; 主函数
;; 加载.h文件，并动态生成scheme定义
(define (load-h-file file-path)
  (let loop ([stm-obj-list (parse file-path)])
    (cond [(null? stm-obj-list) #t]
          [else
           (define-scheme (car stm-obj-list))
           (loop (cdr stm-obj-list))])))

;; parse主函数
(define (parse file-path)
  (with-finish
   close-input-port
   ([ip (open-input-file file-path)])
   (let ([reader (make-reader ip)])
     (let loop ([states all-statements])
       (cond [(null? (peek-word reader) '())]
             [(null? states)
              (begin
                (display-current-near-words)
                (error 'parse "没有语句支持"))]
             [(statement-match? (car states) reader)
              (cons (statement-fill (car states) reader)
                    (loop all-statements))]
             [else (loop (cdr states))])))))


;;;;;;;;;;;;;;;;;; 上下文定义
;; 创建上下文
(define (make-context)
  (let ([ctx (make-eq-hashtable)])
    (lambda (x)
      (record-case
       x
       [(in-table?) (k)
        (hashtable-contains? ctx k)]
       [(val) (k)
        (hashtable-ref ctx k '())]
       [(set-val) (k v)
        (hashtable-set! ctx k v)]
       [(get-ctx) () ctx]))))

;; 是否在上下文中定义
(define (defined? context identifer)
  (context (list 'in-table? identifer)))

;; 获取标识符的值
(define (identifer-value context identifer)
  (context (list 'val identifer)))

;; 设置标识符的值
(define (define-value! context identifer value)
  (context (list 'set-val identifer value)))

;; 上下文
(define ctx (make-context))


;;;;;;;;;;; 语句相关操作 ;;;;;;;;;;;;;;;;
;; 创建空语句
(define (make-empty-token)
  (lambda (x)
    (record-case
     x
     [(type) () 'empty]
     [(match?) (_) #t]
     [(fill) (_) '()])))


;; 是否是空的token
(define (empty-token? stm)
  (eq? 'empty (stm '(type))))

;; 从input-port中取出数据，填充语句
(define (statement-fill stm reader)
  (stm (list 'fill reader)))

;; 检查port是否符合指定的语句，检查之后会恢复ip指针位置
(define (statement-match? stm reader)
  (with-finish
   reset!
   ([treader (make-reader (the-input-port reader))])
   (stm (list 'match? treader))))


;; 创建复合语句-便利接口
(define (make-statement . token)
  (make-statement-by-stream (->lazy token)))

;; 创建复合语句
(define (make-statement-by-stream state-stream)
  (lambda (x)
    (record-case
     x
     [(type) () 'compose]
     [(substms) () state-stream]
     [(match?) (reader)
      (let loop ([states state-stream])
        (cond [(empty-stream? states) #t]
              [(not ((stream-car states) (list 'match? reader))) #f]
              [else (loop (stream-cdr states))]))]
     [(fill) (reader)
      (let loop ([states state-stream])
        (cond [(null? states) '()]
              [else
               (cons (statement-fill (stream-car states) reader)
                     (loop (stream-cdr states)))]))])))



;; 多个语句之间加入一个语句作为分隔
(define (statement-join sep-stm stm)
  (let ([sub (stm '(substms))])
    (if (null? sub)
        stm
        (statement-compose
         (stream-join sep-stm sub)))))


;; 要么是第一个语句，要么是第二个语句，要么是...
(define (maybe-or . states)
  (maybe-or-stream (->lazy states)))

(define (maybe-or-stream states)
  (lambda (x)
    (record-case
     x
     [(type) () 'simple]
     [(match?) (ip)
      (let loop ([states states])
        (cond [(null? states) #f]
              [(statement-match? (car states) ip) #t]
              [else (loop (stream-cdr states))]))]
     [(fill) (ip)
      (let loop ([states states])
        (cond [(null? states) (error 'fill-statment "语法错误")]
              [(statement-match? (car states) ip)
               (statement-fill (car states) ip)]
              [else (loop (stream-cdr states))]))])))

;; 这个or语句，有个致命的确定，那就是执行or之后，拿不到最初那个语句的信息了，比如它原本的type,以及它里面的子元素。


;; 1个或者多个语句
(define (one-or-more stm)
  ;; '(or (stm) (stm stm) (stm stm stm) ...)
  ;; (stm (or stm null) (or stm null) ...)
  ;; 如果有一个,substms是 (stm)，如果是两个, substms是 (stm stm)
  (define noos (cons (null-or-one stm)
                   (delay noos)))
  (lambda (x)
    (record-case
     x
     [(type) () 'compose]
     [(substms) () (cons stm noos)]
     [(match?) (ip)
      (if (statement-match? stm ip) #t #f)]
     [(fill) (ip)
      (let loop ([rs '()])
        (if (statement-match? stm ip)
            (loop (append rs (list (statement-fill stm ip))))
            (if (null? rs)
                (error 'fill-statment "语法错误")
                rs)))])))


;; 0个或者1个语句
(define (null-or-one stm)
  (maybe-or stm
                (make-empty-token)))

;; 0个或者多个语句
(define (null-or-more stm)
  (maybe-or (one-or-more stm)
                (make-empty-token)))


;; 目前支持解析的语句
(define all-statements
  (list c-struct c-define c-if c-expr c-function))


;;;;;;;;; C 语句的定义 ;;;;;;;;;;

(define c-fun-pointer
  (make-statement
   (simple-token #\()
   (simple-token #\()
   (trace-token "return-type")
   (simple-token #\))
   (value-token "pointer")
   (simple-token #\))))

(define c-define
  (make-statement
   (simple-token #\#)
   (simple-token 'define)
   (identifer-token "name")
   (maybe-or
    (trace-token "value")
    (value-token "value")
    (with-named "value" c-fun-pointer))))


(define c-struct
  (make-statement
   (simple-token 'struct)
   (identifer-token "name")
   (simple-token #\{)
   (one-or-more c-type-name-pair)
   (simple-token #\})
   (simple-token #\;)))



(define c-type-name-pair
  (define common-pair-token
    (make-statement
     (null-one (simple-token 'const))
     (identifer-token "type")
     (null-one (simple-token #\* "pointer"))
     (identifer-token "name")
     (null-one (array-token) "name-array")))

  (define anonfun-token
    (make-statement
     (identifer-token #\()
     (identifer-token #\))
     (null-one (simple-token #\* "pointer"))
     (identifer-token "name")))
  (maybe-or
   (common-pair-token)
   (anonfun-token)))

simple-token
array-token

(define c-if
  (syntax-form
   '#if c-condition
   (one-more (form-or c-struct c-define c-if))
   '#endif))

(define c-expr
  (let* ([]))
  (syntax-form
   '( (form-join
       cond-tok
       (one-more
        (form-or name-tok
                 (syntax-form '( name-tok '))))) ')))


(define c-condition
  (let* ([tdefd (syntax-form ('defined '( name-tok ')))]
         [ndefd (syntax-form (cond-tok 'defined '( name-tok ')))]
         [defd (form-or defd not-defd)]))
  (form-join cond-tok (null-more defd)))


(define c-function
  (let* ([param (syntax-form
                 '( (form-join
                     '\,
                     (null-more
                      (form-or
                       (syntax-form
                        name-tok
                        (null-one (syntax-form '*))
                        name-tok
                        (null-one (form-or (syntax-form '[])
                                           (syntax-form '[ val-tok ']))))
                       anonfun))) '))
                anonfun (syntax-form
                         name-tok
                         '( '* name-tok ')
                         param)]))
  (syntax-form name-tok
               (null-one (syntax-form '*))
               name-tok
               param
               '\;))



;;;;;;;;;;;;;;;; token 的定义
;;; 创建一个token
(define (make-token checker converter)
  (lambda (x)
    (record-case
     x
     [(type) () 'stom]
     [(match?) (reader) (checker reader)]
     [(fill) (reader) (converter reader)])))

(define (simple-token s)
  (make-token
   (lambda (reader) (eq? s (read-word reader)))
   (lambda (reader) (begin (read-word reader) '()))))

(define (name-token name token)
  (make-token
   (lambda (reader)
     (token (list 'match? reader)))
   (lambda (reader)
     (cons name (token (list 'fill reader))))))

(define (identifer-token name)
  (name-token
   name
   (make-token
    (lambda (reader)
      (let ([w (read-word reader)])
        (not (c-number? w))))
    (lambda (reader)
      (let ([w (read-word reader)]) w)))))

(define (value-token name)
  (name-token
   name
   (make-token
    (lambda (reader)
      (let ([w (read-word reader)])
        (or (number? w)
            (eq? "0x" (substring (symbol->string w) 0 2)))))
    (lambda (reader)
      (let ([w (read-word reader)])
        (c-symbol->number w))))))

(define trace-token
  (make-token
   (lambda (reader)
     (defined? ctx (read-word reader)))
   (lambda (reader)
     (identifer-value ctx (read-word reader)))))

(define (c-number? w)
  (or (number? w)
      (eq? "0x" (substring (symbol->string w) 0 2))))

(define (c-symbol->number w)
  (if (number? w)
      w
      (string->number
       (list->string
        (cons #\#
              (cdr (string->list
                    (symbol->string w))))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 流的定义
(define-syntax stream-cons
  (syntax-rules ()
    [(_ a d)
     (cons a (delay d))]))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-take n stream)
  (let loop ([i 0] [rs '()] [stream stream])
    (cond [(or (eqv? i n) (null? stream)) rs]
          [else (loop (+ i 1)
                      (append rs (list (car stream)))
                      (force (cdr stream)))])))

(define (stream-map proc stream)
  (if (null? stream)
      '()
      (cons (proc (car stream))
            (delay (stream-map proc (force (cdr stream)))))))


;; 将普通list转换为惰性list
(define (list->stream ls)
  (if (null? ls)
      '()
      (cons (car ls)
            (delay (list->stream (cdr ls))))))


(define (stream-join sep stream)
  (cons (car stream)
        (delay (if (null? (stream-cdr stream))
                   '()
                   (cons sep
                         (delay (stream-interware
                                 sep
                                 (stream-cdr stream))))))))

|#


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
           [(or (char-whitespace? c)
                (eq? #\\ c))
            (begin
              (read-char p)
              (f (peek-char p)))]
           [(char-separator? c)
            (read-char p)]
           [(char-identifier? c)
            (read p)]
           [(eq? #\/ c)
            (read-char p)
            (cond [(eq? #\/ (peek-char p))
                   (begin (read-char-until p #\newline)
                          (f (peek-char p)))]
                  [(eq? #\* (peek-char p))
                   (begin (read-char-until p #\* #\/)
                          (f (peek-char p)))]
                  [else c])]
           [else (f (read-char p))]))]
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
      (eq? #\_ c)
      (eq? #\" c)))

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


#|
(define ip (open-input-file "~/Desktop/tcc-0.9.24/libtcc.h"))

(define reader (make-reader ip))

(display-current-near-words reader)

(read-word reader)
(reset! reader)

(close-input-port ip)
|#


;; 环境
(define env )


;; 解析主函数
(define (parse pattern reader frame)
  (cond [(eof-object? (peek-word reader))
         (error 'parse "eof")]
        [(static-pattern? pattern)
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
         (handle-rule pattern reader frame)]
        [(or-pattern? pattern)
         (or-pattern-handler pattern reader frame)]
        [else (error 'parse "不支持该模式" pattern)]))



(define handle-compose
  (lambda (pattern reader frame)
    (let loop ([ps (cdr pattern)]
               [rs frame])
      (if (null? ps)
          rs
          (loop (cdr ps)
                (parse (car ps) reader rs))))))



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
  (define fecth-fields
    (lambda (fields data)
      (map
       (lambda (e)
         (cons e
               (fold-left
                (lambda (rs x)
                  (if (eq? (car x) e)
                      (append rs (list (cdr x)))
                      rs))
                '()
                data)))
       fields)))
  (append frame
          (list
           (cons (rule-name rule)
                 (fecth-fields
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



(define (static-pattern? p)
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


(define (extend-frame frame e)
  (append frame (list e)))


(define (or-pattern-handler pattern reader frame)
  (append
   frame
   (let ([reader (copy-reader reader)])
     (let loop ([inners (or-pattern-inner pattern)])
       (cond [(null? inners)
              (error 'or-pattern-handler "无匹配模式")]
             [else
              (amb
               (guard (_ [#t (begin (reset! reader) (amb))])
                 (parse (car inners) reader '()))
               (loop (cdr inners)))])))))


(define (or-pattern . patterns)
  (cons 'or-pattern patterns))

(define (or-pattern-inner pattern)
  (cdr pattern))

(define (or-pattern? pattern)
  (eq? 'or-pattern (car pattern)))




(define arg
  (make-rule
   '(p-type p-name p-pointer a-num struct?)
   (compose-pattern
    (b-var 'const 'const)
    (b-var 'struct? 'struct)
    (make-var 'p-type)
    (b-var 'p-pointer #\*)
    (make-var 'p-name)
    (or-more
     (compose-pattern #\[ (make-var 'a-num) #\]))
    )))

(define c-function
  (make-rule
   '(ret ret-pointer fname args)
   (compose-pattern
    (make-var 'ret)
    (b-var 'ret-pointer #\*)
    (make-var 'fname)
    #\(
    (join-repeat-pattern
     #\,
     (arg 'args))
    #\)
    )))


(define c-define
  (make-rule
   '(name val)
   (compose-pattern
    #\#
    'define
    (make-var 'name)
    (make-var 'val))))

(define c-struct
  (make-rule
   '(name args)
   (or-pattern
    (compose-pattern
     'typedef
     'struct
     (make-var 'bname)
     #\{
     (or-more
      (compose-pattern (arg 'args) #\;))
     #\}
     (make-var 'name)
     ))))

(parse
 (or-more
  (compose-pattern (arg 'args) #\;))
 (make-reader
  (open-input-string
   "struct panel *below;
        struct panel *above;
        NCURSES_CONST void *user;"))
 '())

(parse (c-struct 'struct)
       (make-reader
        (open-input-string
         "typedef struct panel
{
 WINDOW *win;
        struct panel *below;
        struct panel *above;
        NCURSES_CONST void *user;
        } PANEL;
"))
'())


(define statement
  (or-pattern (c-function 'function)
              (c-define 'define)
              (c-struct 'struct)))


(parse (or-more statement)
       (make-reader
        (open-input-string
         "int test(char a, int *b)
#define aaa 666
typedef struct panel
{
 WINDOW *win;
        struct panel *below;
        struct panel *above;
        NCURSES_CONST void *user;
        } PANEL;
"))
       '())




#|

(or-pattern-handler
 (or-pattern (compose-pattern 'aaa (make-var 'n) 'bbb)
             (compose-pattern 'aaa 'uut (make-var 'n)))
 (make-reader
  (open-input-string
   "aaa uut bbb ccc"))
 '())

(parse (or-pattern (compose-pattern 'aaa (make-var 'n) 'bbb)
                   (compose-pattern 'aaa 'uut (make-var 'n)))
       (make-reader
        (open-input-string
         "aaa uut bbb ccc"))
       '())


(define statement
  (or-pattern (c-function 'function)
              (c-define 'define)))

(parse (or-more statement))


(define arg
  (make-rule
   '(p-type p-name p-pointer a-num)
   (compose-pattern
    (b-var 'const 'const)
    (make-var 'p-type)
    (b-var 'p-pointer #\*)
    (make-var 'p-name)
    (or-more
     (compose-pattern #\[ (make-var 'a-num) #\]))
    )))

(define c-function
  (make-rule
   '(ret ret-pointer fname args)
   (compose-pattern
    (make-var 'ret)
    (b-var 'ret-pointer #\*)
    (make-var 'fname)
    #\(
    (join-repeat-pattern
     #\,
     (arg 'args))
    #\)
    )))


(define c-define
  (make-rule
   '(name val)
   (compose-pattern
    #\#
    'define
    (make-var name)
    (make-var val))))




(parse (c-function 'function)
       (make-reader
        (open-input-string
         "int test(char a, int *b)"))
       '())


(parse (compose-pattern
        (make-var 'ret)
        (b-var 'ret-pointer #\*)
        (make-var 'fname)
        #\(
        (join-repeat-pattern
         #\,
         (arg 'args))
        #\)
        )
       (make-reader
        (open-input-string
         "int test(char a, int *b)"))
       '())


"int test (char a[2], int b[1])"

(parse (arg 'arg)
       (make-reader
        (open-input-string
         "char *a)"))
       '((a . 1)))

|#

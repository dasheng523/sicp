

(define rt-apply
  (lambda (rt reader)
    (rt reader)))

(define rt-apply-reset
  (lambda (rt reader)
    (rt-apply (fail-reset rt) reader)))


(define default-reader
  (lambda (p)
    (let ([position (file-position p)])
      (lambda (x)
        (record-case
         x
         [(reset-reader!) () (file-position p position)]
         [(the-input-port) () p])))))

(define reset-reader!
  (lambda (reader)
    (reader '(reset-reader!))))

(define the-input-port
  (lambda (reader)
    (reader '(the-input-port))))

(define copy-reader
  (lambda (reader)
    (default-reader (the-input-port reader))))

(define fail-reset
  (lambda (rt)
    (lambda (reader)
      (let ([reader (copy-reader reader)])
        (let ([rs (rt-apply rt reader)])
          (unless rs (reset-reader! reader))
          rs)))))


(define fetch-word
  (lambda (reader)
    (read (the-input-port reader))))

(define fetch-char
  (lambda (reader)
    (read-char (the-input-port reader))))

(define peek
  (lambda (rt)
    (lambda (reader)
      (let ([reader (copy-reader reader)])
        (let ([rs (rt-apply rt reader)])
          (reset-reader! reader)
          rs)))))

(define the-empty-rt
  (lambda (reader) '()))

(define rt-not
  (lambda (rt)
    (lambda (reader)
      (let ([rs (rt-apply-reset rt reader)])
        (if rs #f #t)))))

(define (rt-or . parterns)
  (lambda (reader)
    (if (null? parterns)
        #f
        (let ([rs (rt-apply-reset (car parterns) reader)])
          (if rs
              rs
              (rt-apply-reset (apply rt-or (cdr parterns)) reader))))))


;; 依次查询模式，如果得到#f，就返回#f，直到最后一个模式
(define (rt-and . parterns)
  (lambda (reader)
    (cond [(null? parterns) #t]
          [(null? (cdr parterns))
           (rt-apply-reset (car parterns) reader)]
          [else
           (let ([rs (rt-apply-reset (car parterns) reader)])
             (if rs
                 (rt-apply-reset (apply rt-and (cdr parterns)) reader)
                 #f))])))



(define-syntax delay-rt
  (syntax-rules ()
    [(_ rt)
     (delay rt)]))


(define force-rt
  (lambda (rt)
    (lambda (reader)
      (rt-apply-reset (force rt) reader))))

(define-syntax lazy-rt
  (syntax-rules ()
    [(_ rt)
     (force-rt (delay-rt rt))]))


(define rt-if
  (case-lambda
    [(pred then else)
     (lambda (reader)
       (if (rt-apply-reset pred reader)
           (rt-apply-reset then reader)
           (if else
               (rt-apply-reset else reader)
               #f)))]
    [(pred then)
     (rt-if pred then #f)]))


(define result-handler
  (lambda (rt handler)
    (lambda (reader)
      (handler (rt-apply rt reader)))))

(define rt-check
  (lambda (rt checker)
    (result-handler
     rt
     (lambda (rs)
       (if (checker rs) rs #f)))))

(define cons-rt
  (lambda (rt rest)
    (lambda (reader)
      (let ([rs (rt-apply-reset rt reader)])
        (if rs
            (let ([rests (rt-apply-reset rest reader)])
              (if rests
                  (cons rs rests)
                  #f))
            #f)))))


;; 支持二进制，八进制，十进制，十六进制，正负
(define rt-number
  (letrec* ([fetch-number-list
             (rt-if (rt-check (peek fetch-char)
                              (lambda (c)
                                (and (not (eof-object? c))
                                     (char-numeric? c))))
                    (cons-rt fetch-char
                             (rt-or (lazy-rt fetch-number-list)
                                    (lambda (reader) '())))
                    #f)]
            [rt-oct
             (result-handler
              fetch-number-list
              (lambda (rs)
                (if rs
                    (string->number
                     (list->string
                      (if (eq? #\0 (car rs))
                          (append (list #\# #\o)
                                  (cdr rs))
                          rs)))
                    #f)))]
            [rt-bin
             (result-handler
              (rt-and (rt-fetch-the-char #\0)
                      (rt-or (rt-fetch-the-char #\b)
                             (rt-fetch-the-char #\B))
                      fetch-number-list)
              (lambda (rs)
                (if rs
                    (string->number
                     (list->string
                      (append (list #\# #\b) rs)))
                    #f)))]
            [rt-hex
             (result-handler
              (rt-and (rt-fetch-the-char #\0)
                      (rt-or (rt-fetch-the-char #\x)
                             (rt-fetch-the-char #\X))
                      fetch-number-list)
              (lambda (rs)
                (if rs
                    (string->number
                     (list->string
                      (append (list #\# #\x) rs)))
                    #f)))]
            [rt-com (rt-or rt-hex rt-bin rt-oct)])
    rt-com))



(define rt-eof
  (rt-check fetch-char eof-object?))

(define rt-identifier
  (rt-and
   (rt-check (peek fetch-char)
             (lambda (c)
               (or (char-alphabetic? c)
                   (eq? #\_ c))))
   fetch-word))

(define rt-whitespace
  (rt-and
   (rt-check (peek fetch-char)
             char-whitespace?)
   fetch-char))


(define rt-punctuationm
  (rt-if (rt-and (rt-not (peek rt-number))
                 (rt-not (peek rt-identifier))
                 (rt-not (peek rt-whitespace)))
         fetch-char))

(define rt-fetch-the-char
  (lambda (char)
    (rt-and
     (rt-check (peek fetch-char)
               (lambda (c) (eq? c char)))
     fetch-char)))

(define rt-fetch-the-word
  (lambda (w)
    (rt-and
     (rt-check (peek fetch-word)
               (lambda (c) (eq? c w)))
     fetch-word)))


(define rt-space
  (rt-fetch-the-char #\space))

(define rt-word
  (rt-or
   rt-identifier
   rt-number
   rt-punctuationm))

(define (rt-sequence . rts)
  (lambda (reader)
    (if (null? rts)
        '()
        (let ([rs (rt-apply-reset (car rts) reader)])
          (if rs
              (let ([rest (rt-apply-reset
                           (apply rt-sequence (cdr rts)) reader)])
                (if rest
                    (cons rs rest)
                    #f))
              #f)))))

(define (rt-sequence . rts)
  (if (null? rts)
      the-empty-rt
      (cons-rt (car rts)
               (lazy-rt (apply rt-sequence (cdr rts))))))



(define repeat-until
  (lambda (rt until)
    (rt-if until
           the-empty-rt
           (cons-rt rt
                    (lazy-rt (repeat-until rt until))))))

(define repeat-before
  (lambda (rt until)
    (rt-if until
           (lambda (reader) #f)
           (cons-rt rt
                    (rt-or (lazy-rt (repeat-until rt until))
                           the-empty-rt)))))


(define skip-until
  (lambda (skip rt)
    (rt-if skip
           (lazy-rt (skip-until skip rt))
           rt)))


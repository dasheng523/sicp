(library (lib ring)
  (export bip-parser main-exp)
  (import (lib common)
          (chezscheme))

  (define bip-parser
    (lambda (rt bip)
      (let ([index (file-position bip)])
        (let ([rs (rt bip)])
          (if rs
              rs
              (begin (file-position bip index)
                     #f))))))

  (define handle-rt
    (lambda (rt handle)
      (lambda (bip)
        (let [(rs (bip-parser rt bip))]
          (if rs (handle rs) #f)))))

  (define one-char
    (lambda (bip)
      (let ([u8 (get-u8 bip)])
        (if (eof-object? u8) #f (integer->char u8)))))

  (define one-u8
    (lambda (bip)
      (let ([u8 (get-u8 bip)])
        (if (eof-object? u8) #f u8))))

  (define eof-char
    (lambda (bip)
      (let ([u8 (get-u8 bip)])
        (if (eof-object? u8) u8 #f))))

  (define given-char
    (lambda (c)
      (handle-rt one-char
                 (lambda (ct)
                   (if (equal? ct c) ct #f)))))

  (define given-u8
    (lambda (u8)
      (handle-rt one-u8
                 (lambda (ct)
                   (if (equal? ct u8) ct #f)))))

  (define given-string
    (lambda (s)
      (handle-rt (apply rt-list
                        (map
                         (lambda (e)
                           (given-u8 e))
                         (bytevector->s8-list (string->utf8 s))))
                 (lambda (rs) s))))

  (define rt-not
    (lambda (rt)
      (lambda (bip)
        (not (bip-parser rt bip)))))

  (define (rt-next rt next)
    (lambda (bip)
      (let ([data (bip-parser rt bip)])
        (if data
            (bip-parser (next data) bip)
            #f))))

  (define (rt-nest rt . handlers)
    (if (null? handlers)
        (handle-rt rt values)
        (apply rt-nest
               (rt-next rt (car handlers))
               (cdr handlers))))

  (define rt-repeat-until
    (lambda (repeat-rt until-rt)
      (lambda (bip)
        (if (bip-parser until-rt bip)
            '()
            (let ([data (bip-parser repeat-rt bip)])
              (if data
                  (let ([rdata (bip-parser (rt-repeat-until repeat-rt until-rt) bip)])
                    (if rdata (cons data rdata) #f))
                  #f))))))

  (define rt-repeat-before
    (lambda (repeat-rt until-rt)
      (lambda (bip)
        (if (bip-parser until-rt bip)
            #f
            (let ([data (bip-parser repeat-rt bip)])
              (if data
                  (let ([rdata (bip-parser (rt-repeat-until repeat-rt until-rt) bip)])
                    (if rdata
                        (cons data rdata)
                        (cons data '())))
                  #f))))))

  (define (rt-fold fn rts)
    (lambda (bip)
      (let rt-left ([init '()] [rts rts])
        (if (null? rts)
            init
            (let ([data (bip-parser (car rts) bip)])
              (if data
                  (rt-left (fn init data)
                           (cdr rts))
                  #f))))))


  (define (rt-list . rts)
    (rt-fold (lambda (rs item)
               (append rs (list item)))
             rts))

  (define (rt-append . rts)
    (rt-fold append rts))

  (define (rt-begin . rts)
    (rt-fold
     (lambda (rs item)
       item)
     rts))


  (define u8-list->string
    (lambda (u8)
      (string-trim (utf8->string (u8-list->bytevector u8)))))

  (define match-str-until
    (lambda (until)
      (handle-rt (rt-repeat-until one-u8 until) u8-list->string)))

  (define mark-k
    (lambda (k rt)
      (handle-rt rt
                 (lambda (rs) (list k rs)))))

  (define str-k-rt
    (lambda (k until)
      (mark-k k (match-str-until until))))

  (define rnewline
    (rt-begin (given-char #\return)
              (given-char #\newline)))

  (define method-rt (str-k-rt 'method (given-char #\space)))
  (define path-rt (str-k-rt 'path (given-char #\space)))
  (define protocol-rt (str-k-rt 'protocol rnewline))

  (define header-name-rt
    (handle-rt
     (match-str-until (given-char #\:))
     (lambda (rs)
       (string->symbol (string-downcase rs)))))
  (define header-value-rt (match-str-until rnewline))
  (define header-rt
    (mark-k 'headers
            (rt-repeat-until (rt-list header-name-rt header-value-rt) rnewline)))

  (define body-u8-rt (mark-k 'body (rt-repeat-until one-u8 eof-char)))
  (define body-str-rt (str-k-rt 'body eof-char))


  (define body-form-data
    (lambda (data)
      (define boundary-num
        (let ([content-type (assoc 'content-type
                                   (cadr (assoc 'headers data)))])
          (if content-type
              (car (reverse (string-split (cadr content-type) #\-)))
              (error 'form-data "content-type 为空"))))
      (define boundary-line
        (rt-repeat-until (given-char #\-)
                         (given-string boundary-num)))

      (define parse-attr
        (lambda (rs)
          (let ([kname (car rs)]
                [kval (cadr rs)])
            (let ([sls (string-split kval #\;)])
              (cons
               (list kname (car sls))
               (if (cdr sls)
                   (let loop ([ls (cdr sls)])
                     (map
                      (lambda (lstr)
                        (let ([kvs (string-split lstr #\=)])
                          (if (= (length kvs) 2)
                              (list (string->symbol (string-trim (car kvs)))
                                    (eval-from-str (cadr kvs))))))
                      ls))
                   '()))))))


      (define form-item-attr
        (handle-rt
         (rt-repeat-until
          (handle-rt
           (rt-list header-name-rt header-value-rt)
           parse-attr)
          rnewline)
         (lambda (rs)
           (fold-right (lambda (x ls) (append ls x)) '() rs))))

      (define form-item-body
        (lambda (req)
          (handle-rt
           (rt-repeat-until
            one-u8
            (rt-begin rnewline boundary-line))
           (lambda (rs)
             (list
              (cadr (assoc 'name req))
              (let ([item-type (assoc 'content-type req)])
                (if item-type
                    (open-bytevector-input-port
                     (u8-list->bytevector rs))
                    (u8-list->string rs))))))))

      (define form-item
        (rt-begin
         rnewline
         (rt-nest
          form-item-attr
          form-item-body)))

      (rt-begin
       boundary-line
       (handle-rt
        (mark-k 'body
                (rt-repeat-until form-item (given-string "--")))
        (lambda (rs)
          (append data (list rs)))))))


  (define content-type-route
    (lambda (data)
      (define type
        (let ([content-type (assoc 'content-type
                                   (cadr (assoc 'headers data)))])
          (if content-type (cadr content-type) "")))
      (cond [(string-start-with type "application/x-www-form-urlencoded")
             body-str-rt]
            [(string-start-with type "multipart/form-data")
             (body-form-data data)]
            [else
             body-u8-rt])))

  (define main-exp
    (rt-nest
     (rt-append
      (rt-list method-rt
               path-rt
               protocol-rt)
      (rt-list header-rt))
     content-type-route))


  )

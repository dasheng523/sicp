(library (webserver request)
  (export request-method request-url request-protocol request-body request-headers request-cookies
          request-attributes request-header request-cookie request-attribute
          make-request)
  (import (chezscheme)
          (lib common))


  ;; dataitem
  (define-record-type dataitem (fields key val))

  ;; datapack
  (define make-datapack
    (lambda (ht)
      (lambda (x)
        (record-case
         x
         [(get-item) (k) (hashtable-ref ht k #f)]
         [(with-item) (k v)
          (begin (hashtable-set! ht k v)
                 (make-datapack ht))]))))

  ;; 通过 dataitem 列表构建 datapack
  (define make-datapack-dataitemlist
    (lambda (dataitem-list)
      (make-datapack (list->table dataitem-list))))

  (define list->table
    (lambda (ls)
      (let ([ht (make-eq-hashtable)])
        (for-each
         (lambda (item)
           (hashtable-set!
            ht
            (dataitem-key item)
            (dataitem-val item)))
         ls)
        ht)))


  ;;;; cookies TODO  不用这个实现了
  ;;;; 实现是make-datapack
  (define make-cookies make-datapack)

  (define make-cookies-from-string
    (lambda (inputstr)
      (let* ([strpie (string-split inputstr #\;)]
             [ls (map
                  (lambda (pie)
                    (let ([kv (string-split pie #\=)])
                      (make-dataitem
                       (string->symbol (string-trim (car kv)))
                       (string-trim (cadr kv)))))
                  strpea)])
        (make-datapack-dataitemlist ls))))

  (define make-empty-cookies
    (lambda ()
      (make-cookies (make-eq-hashtable))))

  ;;;; setcookies
  (define make-setcookie
    (case-lambda
      [(name value expires domain path secure? http-only?)
       (lambda (x)
         (record-case
          x
          [(get-name) () name]
          [(get-value) () value]
          [(get-expires) () expires]
          [(get-domain) () domain]
          [(get-path) () path]
          [(get-secure?) () secure?]
          [(get-http-only?) () http-only?]))]
      [(name value)
       (make-setcookie name value #f #f #f #f #f)]
      [(name value expires)
       (make-setcookie name value expires #f #f #f #f)]))


  (define make-setcookies
    (lambda (ls)
      (lambda (x)
        (record-case
         x
         [(append) (k v)
          (make-setcookies (append ls (list (make-setcookie k v))))]))))

  (define make-empty-setcookies
    (lambda ()
      (make-setcookies '())))


  ;;;; headers
  ;;;; 实现是make-datapack TODO
  (define make-headers
    (lambda (ht)
      (let* ([parent (make-datapack ht)]
             [cookies-raw (hashtable-ref ht 'cookie #f)])
        (let create ([cookies (if cookies-raw
                                  (make-cookies-from-string cookies-raw)
                                  (make-empty-cookies))]
                     [setcookies (make-empty-setcookies)])
          (lambda (x)
            (record-case
             x
             [(get-cookies) () cookies]
             [(get-cookie) (k)
              (cookies `(get-item ,k))]
             [(with-cookie) (k v)
              (create (cookies `(with-item ,k ,v))
                      setcookies)]
             [(setcookie) (k v)
              (create cookies
                      (setcookies `(append ,k ,v)))]
             [else (parent x)]))))))


  (define make-headers-from-array
    (lambda (data)
      (make-headers
       (list->table
        (map
         (lambda (item)
           (make-dataitem (car item) (cadr item)))
         data)))))

  (define make-empty-headers
    (lambda ()
      (make-headers (make-eq-hashtable))))


  ;;;; attributes
  ;;;; 实现是make-datapack
  (define make-attributes make-datapack)

  (define make-empty-attributes
    (lambda ()
      (make-attributes (make-eq-hashtable))))



  ;; webdata
  (define make-webdata
    (lambda (body headers attributes)
      (lambda (x)
        (record-case
         x
         [(get-body) () body]
         [(get-headers) () headers]
         [(get-attributes) () attributes]
         [(get-header) (k) (headers `(get-item ,k))]
         [(get-attribute) (k) (attributes `(get-item ,k))]
         [(with-header) (k v)
          (make-webdata body
                        (headers (list 'with-item k v))
                        attributes)]
         [(with-attribute) (k v)
          (make-webdata body headers
                        (attributes (list 'with-item k v)))]))))

  ;;;; request
  (define make-request
    (lambda (method path protocol body headers attributes)
      (let create ([webdata (make-webdata body headers attributes)])
        (lambda (x)
          (record-case
           x
           [(get-method) () method]
           [(get-path) () path]
           [(get-protocol) () protocol]
           [(get-cookies) () (headers `(get-cookies))]
           [(get-cookie) (k) (headers `(get-cookie ,k))]
           [(with-cookie) (k v)
            (make-request method path protocol body
                          (headers `(with-cookie ,k ,v))
                          attributes)]
           [(with-header) (k v)
            (create (webdata (list 'with-header k v)))]
           [(with-attribute) (k v)
            (create (webdata (list 'with-attribute k v)))]
           [else (webdata x)])))))

  (define make-request-from-array
    (lambda (input)
      (define find-value
        (lambda (input k)
          (let ([item (assq k input)])
            (if item
                (cadr item)
                #f))))
      (let* ([method (find-value input 'method)]
             [path (find-value input 'path)]
             [protocol (find-value input 'protocol)]
             [body (find-value input 'body)]
             [headers-raw (find-value input 'headers)])
        (make-request method path protocol body
                      (if headers-raw
                          (make-headers-from-array headers-raw)
                          (make-empty-headers))
                      (make-empty-attributes)))))


  ;;;; response
  (define make-response
    (lambda (code body headers attributes)
      (let create ([webdata (make-webdata body headers attributes)])
        (lambda (x)
          (record-case
           x
           [(get-code) ()
            code]
           [(with-code) (code)
            (make-response code body headers attributes)]


           [(set-cookie) (k v)
            (create (webdata `(set-cookie ,k ,v)))]

           [(with-header) (k v)
            (create (webdata x))]
           [(with-attribute) (k v)
            (create (webdata x))]
           [(with-body) (body)
            (create (webdata x))]


           [else (webdata x)])))))






  (define req
    (make-request-from-array
     '((method "POST")
       (path "/sdfsd")
       (protocol "HTTP/1.1")
       (headers
        ((user-agent "PostmanRuntime/7.20.1") (accept "*/*") (cache-control "no-cache")
         (postman-token "37d487e5-c75e-455f-bc59-db4f9dfb0eaa")
         (host "127.0.0.1:9000")
         (content-type
          "multipart/form-data; boundary=--------------------------654164899475887629483156")
         (accept-encoding "gzip, deflate") (content-length "18686")
         (connection "keep-alive")))
       (body
        (("asd" "你好")
         ("aaaaaa" "6655好里等等"))))))


  (define header (make-headers-from-array
                  '((user-agent "PostmanRuntime/7.20.1") (accept "*/*") (cache-control "no-cache")
                    (postman-token "37d487e5-c75e-455f-bc59-db4f9dfb0eaa")
                    (host "127.0.0.1:9000")
                    (content-type
                     "multipart/form-data; boundary=--------------------------654164899475887629483156")
                    (accept-encoding "gzip, deflate") (content-length "18686")
                    (connection "keep-alive"))))


  (define resp (make-response 200 "ok"
                              (make-headers (make-eq-hashtable))
                              (make-cookies (make-eq-hashtable))
                              (make-attributes (make-eq-hashtable))))

  (define nresp (resp '(with-cookie a val)))

  (nresp '(get-cookie a))
  (nresp '(get-body))

  )

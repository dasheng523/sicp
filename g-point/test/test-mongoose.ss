(library-directories "/Users/huangyesheng/Documents/project/main-world/g-point")
(library-directories "/home/yesheng/work/sicp/g-point")

;; 调用函数的时候，如果需要可变字符串，可以定义它为u8*, 然后使用make-bytevector来作为参数。
;; 如以下：
#;(let ([buff (make-bytevector 1024)])
  (lambda ()
    (let* ([n (c-read client-socket buff 0 (bytevector-length buff))]
           [bv (make-bytevector n)])
      (bytevector-copy! buff 0 bv 0 n)
      (printf "client:~%~a~%server:~%" (utf8->string bv)))))

;; C中带*的参数，最好用void*，且传参的时候直接传指针，不然会报错的。


(import (chezscheme)
        (ffi ffi-utils)
        (lib common))

(define _init
  (begin (load-lib "mongoose")
         (load-lib "mongoose-utils")))

(define MG_EV_HTTP_REQUEST 100)
(define MG_EV_HTTP_REPLY 101)
(define MG_EV_HTTP_CHUNK 102)
(define MG_EV_SSI_CALL 105)
(define MG_EV_SSI_CALL_CTX 106)

(define MG_EV_HTTP_MULTIPART_REQUEST 121)
(define MG_EV_HTTP_PART_BEGIN 122)
(define MG_EV_HTTP_PART_DATA 123)
(define MG_EV_HTTP_PART_END 124)
(define MG_EV_HTTP_MULTIPART_REQUEST_END 125)


(define-ftype mg_str
  (struct [p (* char)] [len size_t]))

(define-ftype mg_mgr
  (struct))

(define-ftype mg_connection
  (struct))


(define-ftype http_message
  (struct
      [message mg_str]
    [body mg_str]
    [method mg_str]
    [uri mg_str]
    [proto mg_str]
    [resp_code int]
    [resp_status_msg mg_str]
    [query_string mg_str]
    [header_names (array 40 mg_str)]
    [header_values (array 40 mg_str)]))


(define mg_mk_str
  (foreign-procedure "mg_mk_str"
                     (string)
                     (& mg_str)))

(define mg_strfree
  (foreign-procedure "mg_strfree"
                     ((* mg_str))
                     void))


;; void mg_mgr_init(struct mg_mgr *mgr, void *user_data);
(define mg_mgr_init
  (foreign-procedure "mg_mgr_init"
                     (void* void*)
                     void))

(define mg_connect_http
  (foreign-procedure "mg_connect_http"
                     (void* void* utf-8 utf-8 utf-8)
                     (* mg_connection)))

(define mg_mgr_poll
  (foreign-procedure "mg_mgr_poll"
                     (void* int)
                     int))

(define mg_mgr_free
  (foreign-procedure "mg_mgr_free"
                     (void*)
                     void))

#|
struct mg_connection *mg_bind(struct mg_mgr *mgr, const char *address,
    MG_CB(mg_event_handler_t handler,
    void *user_data));
|#
(define mg_bind
  (foreign-procedure "mg_bind"
                     (void* string void* void*)
                     void*))

#|
void mg_send_head(struct mg_connection *n, int status_code,
    int64_t content_length, const char *extra_headers);
|#
(define mg_send_head
  (foreign-procedure
   "mg_send_head"
   (void* int integer-64 string)
   void))

#|
void mg_set_protocol_http_websocket(struct mg_connection *nc);
|#
(define mg_set_protocol_http_websocket
  (foreign-procedure
   "mg_set_protocol_http_websocket"
   (void*)
   void))

#|
void mg_send(struct mg_connection *, const void *buf, int len);
|#
(define mg_send_string
  (foreign-procedure
   "mg_send"
   (void* string int)
   void))
(define mg_send
  (foreign-procedure
   "mg_send"
   (void* void* int)
   void))


(define mg_parse_multipart
  (foreign-procedure
   "mg_parse_multipart"
   (u8* size_t u8* size_t u8* size_t void* void*)
   size_t))


#|
;; client
(define handler
  (foreign-callable
   (lambda (conn ev p)
     (when (= MG_EV_HTTP_REPLY ev)
       (let ([msgp (make-ftype-pointer http_message p)])
         (display
          (getstr (ftype-ref http_message (message) msgp))))))
   (void* int void*)
   void))

(lock-object handler)

(define url "http://www.baidu.com")
(mg_connect_http (ftype-pointer-address mgr)
                 (foreign-callable-entry-point handler)
                 url "" "")

|#

;; server

(define (char*->string address)
  (utf8->string
   (apply bytevector
          (let iter ([i 0])
            (let ((bit (foreign-ref 'unsigned-8 address i)))
              (if (= bit 0)
                  '()
                  (cons bit (iter (+ i 1)))))))))

(define (char*->u8 fp len)
  (let ([bt (make-bytevector len)]
        [addr (ftype-pointer-address fp)])
    (let loop ([i 0])
      (if (= i len)
          bt
          (begin
            (bytevector-u8-set!
             bt i
             (foreign-ref 'unsigned-8 addr i))
            (loop (+ i 1)))))))



(define aaa #f)
(define aaa-point #f)

(define server-handler
  (foreign-callable
   (lambda (conn ev p)
     #;(when (= MG_EV_HTTP_MULTIPART_REQUEST ev)
       (println 'request)
       (println (getstr (ftype-ref http_message (message)
                                   (make-ftype-pointer http_message p)))))
     #;(when (= MG_EV_HTTP_PART_BEGIN ev)
       (println 'begin)
       (println (getstr (ftype-ref http_message (message)
                                   (make-ftype-pointer http_message p)))))
     #;(when (= MG_EV_HTTP_REPLY ev)
       (println 'reply))
     #;(when (= MG_EV_HTTP_MULTIPART_REQUEST_END ev)
       (println "end")
       (let ([text "hello world!"])
         (mg_send_head conn 200 (string-length text) "Content-Type: text/plain")
         (mg_send_string conn text (string-length text))))

     (when (= MG_EV_HTTP_REQUEST ev)
       (let ([msgp (make-ftype-pointer http_message p)])
         (let ([u8 (char*->u8 (ftype-ref http_message (message p) msgp)
                              (ftype-ref http_message (message len) msgp))])
           (set! aaa u8)
           (println (utf8->string u8)))
         #;(println (ftype-ref http_message (method p) msgp))
         #;(println (ftype-ref mg_str
                             (len)
                             (ftype-&ref http_message (method len) msgp)))
         #;(println (adfad (getBody msgp)))
         (println "88888")
         #;(println (getstr (getQueryString msgp)))
         #;(set! aaa (getstr2 (ftype-ref http_message (message) msgp)))
         #;(set! aaa-point (getstr3 (ftype-ref http_message (message) msgp)))
         #;(display 666)
         #;(newline)
         #;(let printheader ([i 0])
           (display (ftype-ref http_message (header_names i) msgp))
           (newline)
           (display (ftype-ref http_message (header_values i) msgp))
           (if (< i 5)
               (printheader (+ i 1))))
         (let ([text "hello world!"])
           (mg_send_head conn 200 (string-length text) "Content-Type: text/plain")
           (mg_send_string conn text (string-length text))))))
   (void* int void*)
   void))

(lock-object server-handler)

(define mgr (foreign-alloc 1000))
(mg_mgr_init mgr 0)

(define conn (mg_bind mgr
                      "9000"
                      (foreign-callable-entry-point server-handler)
                      0))

(mg_set_protocol_http_websocket conn)
(mg_mgr_poll mgr 1000)
(mg_mgr_free mgr)


;; 创建Web服务器
(define create-web-server
  (lambda (port handler)
    (let ([mgr #f]
          [is-stop #f]
          [fpoint
           (let ([callback (foreign-callable handler (void* int void*) void)])
             (lock-object callback)
             (foreign-callable-entry-point callback))])
      (define start
        (lambda ()
          (set! mgr (foreign-alloc 1000))
          (set! is-stop #f)
          (mg_mgr_init mgr 0)
          (mg_set_protocol_http_websocket
           (mg_bind mgr port fpoint 0))
          (fork-thread
           (lambda ()
             (let loop []
               (unless is-stop
                 (println (mg_mgr_poll mgr 1000))
                 (loop)))))))
      (define stop
        (lambda ()
          (set! is-stop #t)
          (server-thread #f)
          (mg_mgr_free mgr)))

      (lambda (x)
        (record-case
         x
         [(start) () (start)]
         [(stop) () (stop)])))))

(define start-server
  (lambda (server)
    (server '(start))))

(define stop-server
  (lambda (server)
    (server '(stop))))



(list-tail (bytevector->u8-list aaa) 600)
(bytevector-length aaa)
(define ip (open-bytevector-input-port aaa))

(define disss
  (lambda (ip)
    (display
     (list->string
      (let loop ([rs (list)])
        (let ([u8 (get-u8 ip)])
          (if (eof-object? u8)
              rs
              (loop (append rs (list (integer->char u8)))))))))
    (newline)))

(disss (open-bytevector-input-port
        (u8-list->bytevector (list-tail (bytevector->u8-list aaa) 630))))

(integer->char (get-u8 ip))
(file-position ip)


(define ww (mg_mk_str "33a"))
(ftype-ref mg_str (len) ww)


(define x (make-ftype-pointer mg_str (foreign-alloc 1000)))
(define ww-ss (mg_mk_str x "sdfasdf"))

(getstr x)
(ftype-ref mg_str (len) x)

(foreign-ref 'int aaa-point 0)

(getstr www)

(char*->string (ftype-pointer-address (ftype-ref mg_str (p) x)))


(define (char*->string address)
  (utf8->string
   (apply bytevector
          (let iter ([i 0])
            (let ((bit (foreign-ref 'unsigned-8 address i)))
              (if (= bit 0)
                  '()
                  (cons bit (iter (+ i 1)))))))))





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
               (lambda (rs)
                 s))))

(define rt-not
  (lambda (rt)
    (lambda (bip)
      (not (bip-parser rt bip)))))

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

(define (rt-list . rts)
  (lambda (bip)
    (if (null? rts)
        '()
        (let ([data (bip-parser (car rts) bip)])
          (if data
              (let ([rdata (bip-parser (apply rt-list (cdr rts)) bip)])
                (if rdata (cons data rdata) #f ))
              #f)))))

(define (rt-append . rts)
  (lambda (bip)
    (if (null? rts)
        '()
        (let ([data (bip-parser (car rts) bip)])
          (if data
              (let ([rdata (bip-parser (apply rt-append (cdr rts)) bip)])
                (if rdata (append data rdata) #f ))
              #f)))))

(define (rt-thunk . rts)
  (lambda (bip)
    (cond [(null? rts) #f]
          [(null? (cdr rts)) (bip-parser (car rts) bip)]
          [else
           (if (bip-parser (car rts) bip)
               (bip-parser (apply rt-thunk (cdr rts)) bip)
               #f)])))


(define match-str-until
  (lambda (until)
    (handle-rt (rt-repeat-until one-u8 until)
               (lambda (rs)
                 (string-trim (utf8->string (u8-list->bytevector rs)))))))

(define str-k-rt
  (lambda (k until)
    (handle-rt (match-str-until until)
               (lambda (rs) (list k rs)))))

(define rnewline
  (rt-thunk (given-char #\return)
                  (given-char #\newline)))

(define method-rt (str-k-rt 'method (given-char #\space)))
(define path-rt (str-k-rt 'path (given-char #\space)))
(define protocol-rt (str-k-rt 'protocol rnewline))

(define header-name-rt (match-str-until (given-char #\:)))
(define header-value-rt (match-str-until rnewline))
(define header-rt
  (rt-repeat-until (rt-list header-name-rt header-value-rt) rnewline))
(define boundary-rt
  (handle-rt
   (rt-thunk
    (rt-repeat-before (given-char #\-)
                      (rt-not (given-char #\-)))
    (match-str-until rnewline))
   (lambda (rs)
     (list 'boundary rs))))

(define content-type-rt
  (rt-list header-name-rt (match-str-until (given-string "boundary="))))


(define body-u8-rt (rt-repeat-until one-u8 eof-char))
(define body-str-rt (str-k-rt 'body eof-char))



(define form-data
  (lambda (boundary-num)
    (define boundary-line
      (rt-repeat-until (given-char #\-) (given-string boundary-num)))
    (define attr-rt
      (handle-rt
       (rt-list header-name-rt header-value-rt)
       (lambda (rs)
         (let ([kname (car rs)]
               [kval (cadr rs)])
           (let ([sls (string-split ";" kval)])
             (cons
              (cons kname (car sls))
              (let loop ([ls (cdr sls)])
                )))))))
    (define form-item
      (rt-thunk
       rnewline
       (rt-list
        (rt-repeat-until
         (rt-list header-name-rt header-value-rt)
         rnewline)
        (handle-rt
         (rt-repeat-until one-u8 boundary-line)
         (lambda (rs)
           (list "data" rs))))))
    (rt-thunk
     boundary-line
     (rt-repeat-until form-item (given-string "--")))))

(define bip (open-bytevector-input-port aaa))

(bip-parser (rt-append
             (rt-list method-rt
                      path-rt
                      protocol-rt
                      content-type-rt
                      boundary-rt)
             header-rt
             (form-data "983216484947391279531332"))
            (open-bytevector-input-port aaa)
            #;bip)

(bip-parser (match-str-until (given-char #\space)) bip)

"----------------------------925461089897661364009791\r\nContent-Disposition: form-data; name=\"asd\"\r\n\r\n你好\r\n----------------------------925461089897661364009791\r\nContent-Disposition: form-data; name=\"ffgf\"; filename=\"18763700-8ffe41b3e10f26db.jpg\"\r\nContent-Type: image/jpeg\r\n\r\n����\x0;\x10;JFIF"






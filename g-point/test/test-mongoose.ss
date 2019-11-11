(library-directories "/Users/huangyesheng/Documents/project/main-world/g-point")
(library-directories "/home/yesheng/work/sicp/g-point")

;; 调用函数的时候，如果需要可变字符串，可以定义它为int u8*, 然后使用make-bytevector来作为参数。
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

(define MG_EV_HTTP_REPLY 101)


(define-ftype mg_str
  (struct [p (* char)] [len size_t]))

(define-ftype mg_mgr
  (struct))

(define-ftype mg_connection
  (struct))

(define-ftype http_message
  (struct
      [message (* mg_str)]
    [body (* mg_str)]
    [method (* mg_str)]
    [uri (* mg_str)]
    [proto (* mg_str)]
    [resp_code int]
    [resp_status_msg (* mg_str)]
    [query_string (* mg_str)]
    [header_names (array 40 (* mg_str))]
    [header_values (array 40 (* mg_str))]))

(define mg_mk_str
  (foreign-procedure "mg_mk_str"
                     (string)
                     (* mg_str)))

(define mg_strfree
  (foreign-procedure "mg_strfree"
                     (void*)
                     void))

(define getstr
  (foreign-procedure "getstr"
                     ((* mg_str))
                     string))

;; void mg_mgr_init(struct mg_mgr *mgr, void *user_data);
(define mg_mgr_init
  (foreign-procedure "mg_mgr_init"
                     (void* void*)
                     void))

(define mg_connect_http
  (foreign-procedure "mg_connect_http"
                     ((* mg_mgr) void* utf-8 utf-8 utf-8)
                     (* mg_connection)))

(define mg_mgr_poll
  (foreign-procedure "mg_mgr_poll"
                     ((* mg_mgr) int)
                     int))

(define mg_mgr_free
  (foreign-procedure "mg_mgr_free"
                     ((* mg_mgr))
                     void))


(define url "http://www.baidu.com")

(define (char*->string address)
  (utf8->string
   (apply bytevector
          (let iter ([i 0])
            (let ((bit (foreign-ref 'unsigned-8 address i)))
              (if (= bit 0)
                  '()
                  (cons bit (iter (+ i 1)))))))))


(define mgr (make-ftype-pointer mg_mgr (foreign-alloc 1000)))

(mg_mgr_init (ftype-pointer-address mgr) 0)

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

(mg_connect_http mgr (foreign-callable-entry-point handler) url "" "")

(define is-continue #t)

(define server-thread #f)

(define start-server
  (lambda ()
    (if server-thread
        (set! is-continue #t)
        (set! server-thread
          (fork-thread
           (lambda ()
             (let loop []
               (when is-continue
                 (mg_mgr_poll mgr 1000)
                 (println 'ok)
                 (loop)))))))))

(define stop-server
  (lambda ()
    (set! is-continue #f)))

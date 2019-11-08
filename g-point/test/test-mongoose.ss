(library-directories "/Users/huangyesheng/Documents/project/main-world/g-point")

(import (chezscheme)
        (ffi ffi-utils))

(define _init
  (begin (load-shared-object "../c/mongoose.dylib")))

(define MG_EV_HTTP_REPLY 101)


(define-ftype mg_str
  (struct
      [p (* char)]
    [len size_t]))

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


(define mg_mgr_init
  (foreign-procedure "mg_mgr_init"
                     ((* mg_mgr) void*)
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


(define mgr (make-ftype-pointer mg_mgr (foreign-alloc 1)))

(mg_mgr_init mgr 0)

(define is-continue #t)
(define handler
  (foreign-callable
   (lambda (conn ev p)
     (when (= MG_EV_HTTP_REPLY ev)
       (let ([msgp (make-ftype-pointer
                   http_message
                   (ftype-pointer-address p))])
         (char*->string
          (ftype-ref mg_str (p)
                     (ftype-ref http_message (message) msgp)))))
     (set! is-continue #f))
   ((* mg_connection) int void*)
   void))

(lock-object handler)

(mg_connect_http mgr (foreign-callable-entry-point handler) url "" "")

(let loop []
  (when is-continue
    (mg_mgr_poll mgr 1000)
    (loop)))


(library-directories "/Users/huangyesheng/Documents/project/main-world/g-point")
(library-directories "/home/yesheng/work/sicp/g-point")


(import (chezscheme)
        (ffi mongoose)
        (ffi ffi-utils)
        (lib webserver)
        (lib ring)
        (lib common))



(define aaa #f)
(define conn-info #f)
(define main-handler
  (lambda (conn msg-u8)
    (set! aaa msg-u8)
    (set! conn-info conn)
    "66666"))

(define server (create-web-server "9000" main-handler))

(start-server server)
(poll-server server)

(define bdata (bip-parser main-exp (open-bytevector-input-port aaa)))


(println conn-info)



;; 接下来要处理返回值问题了。
;; 主要是两个函数 response 和 extend-response
(response
 (code 200)
 (cookie 'name "8888" 60)
 (header 'Content-Encoding "gzip")
 (body "hello!!!"))

(extends-response
 data
 (set-header 'content-type "text/html; charset=utf-8"))


;; 这是用来执行response的。
(send-response response)

;; 解析utf8字节，生成request
(parse-request u8)

;; request和response都是webdata的扩展
(get-cookies data)
(get-cookie data key)
(set-cookie data key value expires)
(get-body data)
(set-body data value)

(webdata
 (cookie 'name "8888" 60)
 (body "asdfasdf"))


(define make-simple-expr
  (lambda (k)
    (lambda (content)
      (lambda (x)
        (record-case
         x
         [(type) () k]
         [(get) () content]
         [(set) (new) (set! content new)]
         [(join) (exp)
          (set! content (exp '(get)))])))))

(define make-status-expr
  (lambda (env k)
    (lambda (x)
      (record-case
       x
       [(type) () k]
       [(get) () env]
       [(set) (new) (set! env new)]
       [(join) (exp)
        (let* ([data (exp '(get))]
               [ks (hashtable-keys data)])
          (for-each
           (lambda (k)
             (hashtable-set!
              env
              k
              (hashtable-ref data k #f)))
           ks))]))))

(define body (make-simple-expr 'body))
(define path (make-simple-expr 'path))
(define method (make-simple-expr 'method))
(define protocol (make-simple-expr 'protocol))

(define headers
  (lambda (data)
    (let ([env (make-eq-hashtable)])
      (for-each
       (lambda (item)
         (hashtable-set! env (car item) (cadr item)))
       data)
      (make-status-expr env 'headers))))

(define header
  (lambda (k v)
    (let ([env (make-eq-hashtable)])
      (hashtable-set! env k v)
      (make-status-expr env 'headers))))


(define webdata-maker
  (lambda (ht)
    (lambda (exprs)
      (for-each
       (lambda (e)
         (let ([efn (hashtable-ref ht (e '(type)) #f)])
           (if efn
               (efn `(join ,e))
               (hashtable-set! ht (e '(type)) e))))
       exprs)
      ht)))


(define (webdata . exprs)
  ((webdata-maker (make-eq-hashtable)) exprs))

(define (extends-webdata wd . exprs)
  ((webdata-maker wd) exprs))


(define expr-dipatch
  (lambda (wd type op)
    ((hashtable-ref wd type #f) op)))

(define (get-body data)
  (expr-dipatch data 'body '(get)))


;; 主要是一些默认值
(define (response . exprs)
  (let ([init-expr (list (code 200))])
    (apply webdata (append init-expr exprs))))


(define request webdata)


;; 1. 优化：那些get,set函数是不是可以不用写。
;; 2. 怎么转换为具体函数？

(define parse-request
  (lambda (bdata)
    (apply request
           (map
            (lambda (item)
              ((eval (car item)) (cadr item)))
            bdata))))


(define adfff (parse-request bdata))
(get-body adfff)

('method "post")
(method "POST")

(request
 (method "POST"))



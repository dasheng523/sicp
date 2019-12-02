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
    (send-response
     (response
      (header 'server "unknow")
      (header 'content-type "text/plain")
      (body "hello my application")))))

(define server (create-web-server "9000" main-handler))

(start-server server)
(poll-server server)

(define bdata (bip-parser main-exp (open-bytevector-input-port aaa)))

#;(send-response
 (response
  (header 'server "unknow")
  (header 'content-type "text/plain")
  (body "hello my application")))

(println conn-info)







;; common 
(define (make-map . kvs)
  (let ([ht (make-hash-table)])
    (let loop ([kvs kvs])
      (if (null? kvs)
          ht
          (begin (hashtable-set! ht (car kvs) (cadr kvs))
                 (loop (cddr kvs)))))))

(define (map-get m key)
  (hashtable-ref m key #f))

(define (map-set m key val)
  (hashtable-set! m key val))




(define (app-eval webdata expr)
  (expr webdata))

(define (rt-apply callback . rts)
  (lambda (webdata)
    (apply callback
           (let eval-rts ([rts rts])
             (if (null? rts)
                 '()
                 (cons (app-eval webdata (car rts))
                       (eval-rts (cdr rts))))))))

(define-syntax rt-with-eval
  (syntax-rules ()
    [(_ () b1 b2 ...)
     (rt-apply (lambda () b1 b2 ...))]
    [(_ ([i1 e1] [i2 e2] ...) b1 b2 ...)
     (rt-apply
      (lambda (i1 i2 ...) b1 b2 ...)
      e1 e2 ...)]))

(rt-with-eval
 ([path (get-path)]
  [method (get-method)])
 1)

(define (rt-eval . rts)
  (apply rt-apply
         (lambda (n ... last) last)
         rts))

(define (rt-> . rts)
  (lambda (webdata)
    (if (null? rts)
        webdata
        (app-eval
         (app-eval webdata (car rts))
         (apply (cdr rts))))))


;; webdata 使用map来表示
(define create-webdata make-map)

(define webdata-get map-get)

(define webdata-set map-set)


(define the-empty-webdata
  values)


;; 不管request,response,还是web-eval或者其他，求值器的样子都是一样的。所以将它合并成一个求值器。
;; TODO 可能有某些策略，可以将解释器区分开来。
;; TODO body-convert找个机会抽出来。
;; TODO 让实例跑起来
;; TODO 整理所有未实现的表达式，实现它。

(define with-params
  (lambda (k v)
    (lambda (webdata)
      (webdata-set webdata k v))))

(define get-params
  (lambda (k)
    (lambda (webdata)
      (webdata-get webdata k))))

(define (response . exprs)
  (apply rt-eval the-empty-webdata exprs))

(define (with-body body)
  (with-params 'body body))

(define (with-header header-k header-v)
  (with-params header-k header-v))

(define get-method
  (get-params 'method))

(define (get-header k)
  (get-params k))

(define (get-body)
  (get-params 'body))

(define (get-path)
  (get-params 'path))


(define middleware1
  (lambda (handler)
    (rt->
     (with-header 'mw1 "mw1")
     (with-header 'mw2 "mw2")
     handler)))

(define middleware2
  (lambda (handler)
    (rt->
     handler
     (without-cookie 'mw1)
     (with-cookie 'name
                  (string-append (rt-eval (get-cookie 'name)) "-test")))))

(define exception-middleware
  (lambda (handler)
    (lambda (webdata)
      (guard (x [else (begin
                        (log-error x)
                        (exception-handler webdata))])
        (app-eval webdata handler)))))

(define log-error
  (lambda (x)
    (println x)))

(define exception-handler
  (response
   (with-body "error")))


(define create-routes-handler
  (lambda (route-group)
    (rt-with-eval
     ([path (get-path)]
      [method (get-method)])
     (let find-handler ([routes (routes-group-list routes-group)])
       (if (null? routes)
           (routes-group-default routes-group)
           (let ([route (car routes)])
             (if (path-match? path method route)
                 (route-handler route)
                 (find-handler (cdr routes)))))))))


;; route
(define make-route
  (lambda (method pattern handler)
    (make-map
     'method method
     'pattern pattern
     'handler handler)))

(define route-method
  (lambda (route)
    (map-get route 'method)))

(define route-pattern
  (lambda (route)
    (map-get route 'pattern)))

(define route-handler
  (lambda (route)
    (map-get route 'handler)))

;; routes-group
(define make-routes-group
  (lambda (routes default)
    (make-map
     'list routes
     'default default)))

(define routes-group-list
  (lambda (group)
    (map-get group 'list)))

(define routes-group-default
  (lambda (group)
    (map-get group 'default)))

(define page404-handler
  (response
   (with-body "404")))

(define routes
  (make-routes-group
   (list
    (make-route 'GET "/home" (response (with-body "home")))
    (make-route 'POST "/login" (response (with-body "login"))))
   page404-handler))


(define handlers
  (->> (create-routes-handler routes)
       middleware1
       middleware2
       exception-middleware
       ))




;; 上层代码
(define main-app handlers)
(app-eval webdata main-app)














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
          (vector-for-each
           (lambda (k)
             (hashtable-set!
              env
              k
              (hashtable-ref data k #f)))
           ks))]))))

(define code (make-simple-expr 'code))
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


(define (response . exprs)
  (let* ([init-expr (list (code 200))])
    (apply webdata (append init-expr exprs))))

(define request webdata)


;; 1. 优化：那些get,set函数是不是可以不用写。
;; 3. 想写一个OOP设施。因为里面确实用到不少这方面的思想。
;; 4. 测试extends-webdata
;; 5. headers的实现需要修改，因为可以支持多个set-cookie字段的。换句话说header是支持重复字段的。
;; 6. 我太在乎实现形式了。只要有对应的函数，就可以确定一个对象了。

(define parse-request
  (lambda (bdata)
    (apply request
           (map
            (lambda (item)
              ((eval (car item)) (cadr item)))
            bdata))))



(define webdata-get
  (lambda (wd type)
    (expr-dipatch wd type '(get))))

;; 应该就是转化为字节流
;; 1. string-join很消耗性能.
;; 2. 这也是解释器，可以用解释器思维。
;; 有没有跟自定义port结合的地方？
;; 是不是可以合并起来？
;; 为什么要以这种方式作为程序界面？
;; 这里应该用字符串先拼接，然后再转换u8
;; cookie还没有实现
;; 相当于apply一样，应该在过程构造完成才使用数据进行调用，可以提高很大的性能。
(define wnewline "\r\n")
(define send-response
  (lambda (response)
    (let* ([headers (webdata-get response 'headers)]
           [body (->u8 (make-u8-trans (webdata-get response 'body)))]
           [code (webdata-get response 'code)])
      (append-u8
       (->u8 (append-trans
              (make-u8-trans
               (string-append "HTTP/1.1 "
                              (code-msg code)
                              wnewline
                              "Content-Length: "
                              (number->string (bytevector-length body))
                              wnewline))
              (make-u8-trans headers)
              (make-u8-trans wnewline)))
       body))))


(define code-msg
  (lambda (code)
    (let* ([mapstr
            '((100  "Continue")
              (101  "Switching Protocols")
              (200  "OK")
              (201  "Created")
              (202  "Accepted")
              (203  "Non-Authoritative Information")
              (204  "No Content")
              (205  "Reset Content")
              (206  "Partial Content")
              (300  "Multiple Choices")
              (301  "Moved Permanently")
              (302  "Found")
              (303  "See Other")
              (304  "Not Modified")
              (305  "Use Proxy")
              (306  "Unused")
              (307  "Temporary Redirect")
              (400  "Bad Request")
              (401  "Unauthorized")
              (402  "Payment Required")
              (403  "Forbidden")
              (404  "Not Found")
              (405  "Method Not Allowed")
              (406  "Not Acceptable")
              (407  "Proxy Authentication Required")
              (408  "Request Time-out")
              (409  "Conflict")
              (410  "Gone")
              (411  "Length Required")
              (412  "Precondition Failed")
              (413  "Request Entity Too Large")
              (414  "Request-URI Too Large")
              (415  "Unsupported Media Type")
              (416  "Requested range not satisfiable")
              (417  "Expectation Failed")
              (500  "Internal Server Error")
              (501  "Not Implemented")
              (502  "Bad Gateway")
              (503  "Service Unavailable")
              (504  "Gateway Time-out")
              (505  "HTTP Version not supported"))]
           [find (assoc code mapstr)])
      (if find
          (cadr find)
          "Unknow"))))


(define (->u8 trans) (trans))

(define (make-u8-trans obj)
  (cond [(symbol? obj)
         (symbol-u8-trans obj)]
        [(string? obj)
         (string-u8-trans obj)]
        [(number? obj)
         (number-u8-trans obj)]
        [(char? obj)
         (char-u8-trans obj)]
        [(hashtable? obj)
         (hashtable-u8-trans obj)]
        [(textual-port? obj)
         (textual-port-trans obj)]
        [(binary-port? obj)
         (binary-port-trans obj)]
        [else (error 'u8-trans "该对象不支持转换" obj)]))

(define symbol-u8-trans
  (lambda (obj)
    (string-u8-trans (symbol->string obj))))

(define string-u8-trans
  (lambda (obj)
    (lambda ()
      (string->utf8 obj))))

(define number-u8-trans
  (lambda (obj)
    (string-u8-trans (number->string obj))))

(define char-u8-trans
  (lambda (obj)
    (string-u8-trans (make-string 1 obj))))

(define hashtable-u8-trans
  (lambda (obj)
    (fold-left
     (lambda (rs item)
       (append-trans
        rs
        (append-trans (make-u8-trans item)
                      (string-u8-trans ": ")
                      (make-u8-trans (hashtable-ref obj item ""))
                      (string-u8-trans "\r\n"))))
     (string-u8-trans "")
     (vector->list (hashtable-keys obj)))))

(define (append-u8 . u8s)
  (u8-list->bytevector
   (apply append
          (map bytevector->u8-list u8s))))

(define (append-trans . trans-list)
  (lambda ()
    (apply append-u8
           (map ->u8 trans-list))))

(define textual-port-trans
  (lambda (obj)
    (string-u8-trans (get-string-all obj))))

(define binary-port-trans
  (lambda (obj)
    (lambda ()
      (get-bytevector-all obj))))







(->u8 (make-u8-trans "sdf"))

(let ([ht (make-eq-hashtable)])
  (hashtable-set! ht 'aa "aa")
  (hashtable-set! ht 'bb 'bb)
  (hashtable-set! ht "asdfasdf" 20202)
  (->u8 (make-u8-trans ht)))

(->u8 (make-u8-trans 20202))
(->u8 (make-u8-trans (open-string-input-port "sdf")))

(->u8 (make-u8-trans (cadr (assoc "ffgf" (webdata-get adfff 'body)))))



(define adfff (parse-request bdata))

(send-response adfff)

(webdata-get adfff 'body)

(body '(type))

('method "post")
(method "POST")

(request
 (method "POST"))



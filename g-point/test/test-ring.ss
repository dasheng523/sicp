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

(bip-parser main-exp (open-bytevector-input-port aaa))

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

;; webdata应该支持任意扩展。request和response只是webdata的实例罢了，顶多在上面封装一些通用的信息。

(assoc
 'a
 '( (a 1) (a 2) (b 3) ))

;; 该考虑实现了。。。
;; 我思考的是response问题跟webdata的区别是什么？该怎么设计webdata？
;;   header和cookie是否应该单独定义对象出来？肯定啊。独立出来有什么不对？就不能简单设置response了。
;;   所以在构造函数应该使用项，而在get,set函数使用对象。
;; 可能关键在于选择handler函数。通过hashtable来选择吗？有没有别的方式？可以使用hashtable，只是还应在item上处理多个叠加的情况。
;; 先写伪代码吧
;; 还是挺纠结hashtable的，感觉不熟悉了，不够lisp。但性能问题往往是很重要的一点。
;; 写 item 吧
;; 想起垠神说类型也是解释器。我感觉还是回归这个思路吧。
;; 这样思考下去，body cookie header只是表达式，而response是解释器，它解释得到的结果是一个函数，这个函数可以被调用，以获取函数内部的内容。
;; 我困惑的是如何组合表达式而已。
;; 还有一个疑惑，今天看到某个原则，尽量确保接口足够单一，也就是说如果某些类型不使用这些接口，那么就需要重新定义接口了。
;; 假如我有一个接口 (lambda (type) 1)
;; 这个接口压根不用type，那么这接口我需要怎么定义呢？这个原则还是需要考虑的，往往意味着设计不太正确。我顾不上这个原则。还是算了吧。
;; 感觉这不算解释器，而只是简单的定义和get,set罢了。

;; 有点想设计一个interface类型。


(define body
  (lambda (content)
    (lambda (type)
      (cond [(eq? type 'type) 'body]
            [(eq? type 'init) #f]
            [(eq? type 'get) content]
            [(eq? type 'set)
             (lambda (new)
               (set! content new))]))))

(define header
  (let ([ht #f])
    (lambda (k v)
      (lambda (type)
        (if ht (hashtable-set! ht k v))
        (cond [(eq? type 'type) 'header]
              [(eq? type 'init)
               (begin (set! ht (make-eq-hashtable))
                      (hashtable-set! ht k v))]
              [(eq? type 'get)
               (lambda (k)
                 (hashtable-ref ht k #f))]
              [(eq? type 'set)
               (lambda (k v)
                 (hashtable-set! ht k v))])))))


(define (webdata . exprs)
  (let ([ht (make-eq-hashtable)])
    (for-each
     (lambda (e)
       (if (not (hashtable-ref ht (e 'type) #f))
           (e 'init))
       (hashtable-set! ht (e 'type) e))
     exprs)
    (let ([find-fn
           (lambda (exprs type)
             (hashtable-ref ht type #f))])
      (lambda (type op)
        (let ([fn (find-fn exprs type)])
          (if fn
              ((find-fn exprs type) op)
              (error 'webdata "req不支持该信息" type)))))))

(define (get-body data)
  (data 'body 'get))

(define (get-header data k)
  ((data 'header 'get) k))

(define (set-header data k v)
  ((data 'header 'set) k v))



(define baaa
  (webdata
   (body "sadfas")
   (header 'info "info")
   (header 'data "data")))


(get-body baaa)
(get-header baaa 'info)
(get-header baaa 'data)


;; 这套方案，body，header都是表达式，它们求值之后得到函数。该函数只是做简单的分发。
;; 而webdata也仅仅是对这些表达式进行分类和分发，而get-body，get-header是对这些分发应用。主要逻辑还是写在表达式里。也就是说一切都为了转发，从而获取真正可以调用的函数。
;; 这里的求值函数到底是什么呢？其实就是webdata，将表达式求值为一个分发函数而已。
;; 类型的求值，主要就是做分发。
;; 该方案有个问题，header函数里状态是全局的，应该限定在webdata里才行。

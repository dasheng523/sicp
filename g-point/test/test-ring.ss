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
 (set-cookie 'name "8888")
 (body "hello!!!"))

(extend-response
 response
 (set-header 'content-type "text/html; charset=utf-8"))

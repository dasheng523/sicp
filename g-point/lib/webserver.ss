(library (lib webserver)
  (export create-web-server
          start-server
          stop-server
          poll-server
          ev-data->request
          ev-data->header
          body-fetcher
          char*->string2
          reply
          write-chunk)
  (import (chezscheme)
          (lib common)
          (common combinator)
          (common utils)
          (ffi mongoose)
          (ffi ffi-utils))

  (define init-mgr
    (lambda ()
      (let ([mgr (foreign-alloc 1000)])
        (mg_mgr_init mgr)
        mgr)))


  (define send-text
    (lambda (conn text)
      (mg_printf conn text)))

  (define (reply code headers text)
    (lambda (conn)
      (mg_http_reply conn code headers text)))

  (define (write-chunk buf len)
    (lambda (conn)
      (mg_http_write_chunk conn buf len)))



  (define char*->string2
    (compose utf8->string char*->u8))



  ;; 获取ev-data的request数据
  (define (ev-data->request ev-data)
    (let ([hmsg (make-ftype-pointer mg_http_message ev-data)])
      (let ([method (char*->string2 (ftype-ref mg_http_message (method p) hmsg)
                                    (ftype-ref mg_http_message (method len) hmsg))]
            [uri (char*->string2 (ftype-ref mg_http_message (uri p) hmsg)
                                 (ftype-ref mg_http_message (uri len) hmsg))]
            [query (char*->string2 (ftype-ref mg_http_message (query p) hmsg)
                                   (ftype-ref mg_http_message (query len) hmsg))]
            [proto (char*->string2 (ftype-ref mg_http_message (proto p) hmsg)
                                   (ftype-ref mg_http_message (proto len) hmsg))])
        (make-map
         'method method
         'uri uri
         'query query
         'proto proto))))

  ;; 获取ev-data的header数据
  (define (ev-data->header ev-data)
    (let ([hmsg (make-ftype-pointer mg_http_message ev-data)])
      (let loop ([i 0] [rs '()])
        (let ([len (ftype-ref mg_http_message (headers i name len) hmsg)])
          (if (= len 0)
              rs
              (loop (+ i 1)
                    (let ([name (char*->string2 (ftype-ref mg_http_message (headers i name p) hmsg)
                                                (ftype-ref mg_http_message (headers i name len) hmsg))]
                          [value (char*->string2 (ftype-ref mg_http_message (headers i value p) hmsg)
                                                 (ftype-ref mg_http_message (headers i value len) hmsg))])
                      (cons (cons name value) rs))))))))

  ;; body获取器
  (define (body-fetcher f)
    (lambda (ev-data)
      (let ([hmsg (make-ftype-pointer mg_http_message ev-data)])
        (let ([body (f (ftype-ref mg_http_message (body p) hmsg)
                       (ftype-ref mg_http_message (body len) hmsg))])
          body))))


  (define (make-cb handler)
    (lambda (conn ev ev-data fn-data)
      ((handler conn ev-data) conn)))



  (define (wrapcb-httpmsg cb)
    (lambda (conn ev ev-data fn-data)
      (if (= ev MG_EV_HTTP_MSG)
          (cb conn ev ev-data fn-data))))


  (define (wrapcb-compose . cbs)
    (apply compose cbs))


  (define (server-handler2 handler)
    (wrapcb-httpmsg
     (make-cb handler)))

  ;; 创建回调函数的指针入口
  (define (make-handler-cb handler)
    (let ([callback (create_event_handler (server-handler2 handler))])
      (lock-object callback)
      (foreign-callable-entry-point callback)))

  (define create-web-server
    (lambda (url handler)
      (let ([mgr #f]
            [is-stop #f]
            [fpoint (make-handler-cb handler)])
        (define start
          (lambda ()
            (set! mgr (init-mgr))
            (set! is-stop #f)
            (mg_http_listen mgr url fpoint mgr)
            ;; 在emacs里面是有问题的，还是后续看看
            #;(fork-thread
             (lambda ()
               (let loop []
                 (unless is-stop
                   (mg_mgr_poll mgr 1000)
                   (loop)))))))
        (define stop
          (lambda ()
            (set! is-stop #t)
            (mg_mgr_free mgr)
            (set! mgr #f)))

        (lambda (x)
          (record-case
           x
           [(start) ()
            (if (not is-stop)
                (begin
                  (start)
                  (println "启动完毕"))
                (println "服务器正在运行..."))]
           [(stop) ()
            (if is-stop
                (println "服务器已经停止...")
                (begin
                  (stop)
                  (println "停止完毕")))]
           [(poll) ()
            (mg_mgr_poll mgr 1000)])))))

  (define start-server
    (lambda (server)
      (server '(start))))

  (define stop-server
    (lambda (server)
      (server '(stop))))

  (define poll-server
    (lambda (server)
      (server '(poll))))

  )



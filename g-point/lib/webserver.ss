(library (lib webserver)
  (export create-web-server start-server stop-server poll-server)
  (import (chezscheme)
          (lib common)
          (ffi mongoose)
          (ffi ffi-utils))

  (define init-mgr
    (lambda ()
      (let ([mgr (foreign-alloc 1000)])
        (mg_mgr_init mgr 0)
        mgr)))

  (define get-conn-ip
    (lambda (conn)
      (let* ([buff (make-bytevector 100)]
             [n (mg_conn_addr_to_str
                   conn buff 100
                   (logior MG_SOCK_STRINGIFY_REMOTE MG_SOCK_STRINGIFY_IP))]
             [bv (make-bytevector n)])
        (bytevector-copy! buff 0 bv 0 n)
        (utf8->string bv))))

  (define server-handler
    (lambda (handler)
      (foreign-callable
       (lambda (conn ev p)
         (when (= MG_EV_HTTP_REQUEST ev)
           (let ([msgp (make-ftype-pointer http_message p)])
             (let ([msg-u8 (char*->u8 (ftype-ref http_message (message p) msgp)
                                  (ftype-ref http_message (message len) msgp))]
                   [conni (list (cons 'ip (get-conn-ip conn)))])
               (let ([text (handler conni msg-u8)])   ;; TODO 这里不一定字符串，应该扩展为其他类型。
                 (mg_send_head conn 200 (string-length text) "Content-Type: text/plain")
                 (mg_send_string conn text (string-length text)))))))
       (void* int void*)
       void)))

  (define create-web-server
    (lambda (port handler)
      (let ([mgr #f]
            [is-stop #f]
            [fpoint
             (let ([callback (server-handler handler)])
               (lock-object callback)
               (foreign-callable-entry-point callback))])
        (define start
          (lambda ()
            (set! mgr (init-mgr))
            (set! is-stop #f)
            (mg_set_protocol_http_websocket
             (mg_bind mgr port fpoint 0))
            #;(fork-thread
            (lambda ()
            (let loop []
            (unless is-stop
            (println (mg_mgr_poll mgr 1000))
            (loop)))))))
        (define stop
          (lambda ()
            (set! is-stop #t)
            (mg_mgr_free mgr)))

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



(import (chezscheme)
        (ffi mongoose))

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

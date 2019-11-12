(library (ffi mongoose)
  (export MG_EV_HTTP_REQUEST MG_EV_HTTP_REPLY MG_EV_HTTP_CHUNK MG_EV_SSI_CALL MG_EV_SSI_CALL_CTX
          mg_str mg_mgr mg_connection http_message mg_mk_str mg_strfree
          getstr mg_connect_http mg_mgr_init mg_mgr_free mg_mgr_poll mg_bind
          mg_send_head mg_set_protocol_http_websocket mg_send mg_send_string)
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


  #|
  (define mgr (make-ftype-pointer mg_mgr (foreign-alloc 1000)))
  (mg_mgr_init (ftype-pointer-address mgr) 0)


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


  ;; server
  (define server-handler
  (foreign-callable
  (lambda (conn ev p)
  (when (= MG_EV_HTTP_REQUEST ev)
  (let ([msgp (make-ftype-pointer http_message p)])
  (display
  (getstr (ftype-ref http_message (message) msgp)))
  (let ([text "hello world!"])
  (mg_send_head conn 200 (string-length text) "Content-Type: text/plain")
  (mg_send_string conn text (string-length text))))))
  (void* int void*)
  void))

  (lock-object server-handler)

  (define conn (mg_bind (ftype-pointer-address mgr)
  "9000"
  (foreign-callable-entry-point server-handler)
  0))

  (mg_set_protocol_http_websocket conn)
  (mg_mgr_poll (ftype-pointer-address mgr) 1000)

  |#





  )

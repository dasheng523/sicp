(library (ffi mongoose)
  (export mg_str mg_mgr mg_connection mg_http_header mg_http_message
          mg_mgr_init mg_mgr_poll mg_mgr_free mg_listen mg_http_listen
          mg_send mg_printf mg_http_match_uri create_event_handler
          mg_http_reply mg_http_serve_file mg_http_write_chunk
          MG_EV_ERROR
          MG_EV_POLL
          MG_EV_RESOLVE
          MG_EV_CONNECT
          MG_EV_ACCEPT
          MG_EV_READ
          MG_EV_WRITE
          MG_EV_CLOSE
          MG_EV_HTTP_MSG
          MG_EV_HTTP_CHUNK
          MG_EV_WS_OPEN
          MG_EV_WS_MSG
          MG_EV_WS_CTL
          MG_EV_MQTT_CMD
          MG_EV_MQTT_MSG
          MG_EV_MQTT_OPEN
          MG_EV_SNTP_TIME
          MG_EV_USER)
  (import (chezscheme)
          (ffi ffi-utils)
          (lib common))

  (define _init
    (begin (load-lib "mongoose")))


  (define-ftype mg_str
    (struct [p (* char)] [len size_t]))

  (define-ftype mg_mgr
    (struct))

  (define-ftype mg_connection
    (struct))

  (define-ftype mg_http_header
    (struct
      [name mg_str]
      [value mg_str]))

  (define-ftype mg_http_message
    (struct
      [method mg_str]
      [uri mg_str]
      [query mg_str]
      [proto mg_str]
      [headers (array 40 mg_http_header)]
      [body mg_str]
      [head mg_str]
      [chunk mg_str]
      [message mg_str]))

  (define-enum-ftype mg_ev
    (MG_EV_ERROR
     MG_EV_POLL
     MG_EV_RESOLVE
     MG_EV_CONNECT
     MG_EV_ACCEPT
     MG_EV_READ
     MG_EV_WRITE
     MG_EV_CLOSE
     MG_EV_HTTP_MSG
     MG_EV_HTTP_CHUNK
     MG_EV_WS_OPEN
     MG_EV_WS_MSG
     MG_EV_WS_CTL
     MG_EV_MQTT_CMD
     MG_EV_MQTT_MSG
     MG_EV_MQTT_OPEN
     MG_EV_SNTP_TIME
     MG_EV_USER)
    int)



  (define mg_mgr_init
    (foreign-procedure "mg_mgr_init"
                       (void*)
                       void))

  (define mg_mgr_poll
    (foreign-procedure "mg_mgr_poll"
                       (void* int)
                       void))

  (define mg_mgr_free
    (foreign-procedure "mg_mgr_free"
                       (void*)
                       void))


  (define mg_listen
    (foreign-procedure "mg_listen"
                       (void* string void* void*)
                       void*))

  (define mg_http_listen
    (foreign-procedure "mg_http_listen"
                       (void* string void* void*)
                       void*))

  (define mg_send
    (foreign-procedure
     "mg_send"
     (void* void* int)
     int))

  (define mg_printf
    (foreign-procedure
     "mg_printf"
     (void* string)
     int))

  (define mg_http_match_uri
    (foreign-procedure
     "mg_http_match_uri"
     (void* string)
     boolean))

  (define create_event_handler
    (lambda (f)
      (foreign-callable f
                        (void* int void* void*)
                        void)))

  (define mg_http_reply
    (foreign-procedure
     "mg_http_reply"
     (void* int string string)
     void))

  (define mg_http_serve_file
    (foreign-procedure
     "mg_http_serve_file"
     (void* void* string string string)
     void))

  (define mg_http_write_chunk
    (foreign-procedure
     "mg_http_write_chunk"
     (void* void* int)
     void))


  )

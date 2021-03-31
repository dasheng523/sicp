(load "../startup.ss")

(import (lib webserver))
(import (lib common))
(import (lib json))

(define (encode-headers headers)
  (string-join (map (lambda (h) (string-append (->string (car h)) ": " (->string (cdr h)) "\r\n"))
                    headers)
               ""))

(define server
  (create-web-server
   "http://127.0.0.1:8888"
   (lambda (conn data)
     (display (->string ((body-fetcher char*->string2) data)))
     (reply 200
            (encode-headers (list (cons 'a 123) (cons 'b 666)))
            (->string ((body-fetcher char*->string2) data))))))

(start-server server)

(stop-server server)

(poll-server server)

(define-syntax try
  (syntax-rules ()
    [(_ e1 e2 ...)
     (guard (x [(error? x) (begin (display "aaa:") (display (->string x)) #f)]) e1 e2 ...)]))


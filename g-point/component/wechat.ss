(library (component wechat)
  (import (lib encrypt)
          (lib curler)
          (lib json)
          (lib common)
          (chezscheme))



  )

(define AppID "wx0da9a07ff65da935")
(define AppSecret "60b53fb7e5ed254a7626d81b2ae248d1")
(define Token "1907424487")
(define EncodingAESKey "M93SYG9v0uWMOyW94ocHLakTXkSF5P0i72mSvUa0C6y")

(define (check-signature signature timestamp nonce)
  (= signature
     (sha1
      (apply string-append
             (list-sort string<?
                        (list timestamp nonce Token))))))


(define (retry-on-error thunk)
  (let retry ([i 1] [times 3])
    (with-exception-handler
        (lambda (x)
          (cond [(>= i times)
                 (begin (log "出错了" (->string x))
                        (raise x))]
                [else (retry (+ i 1) times)]))
      thunk)))

(define (get-access-token appid secret)
  (let* ([url (string-append "https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&appid=" appid "&secret=" secret)])
    (retry-on-error
     (lambda ()
       (string->json
        (simple-get url '()))))))


(define (log msg . data)
  (println
   (current-date-format) " "
   msg " "
   (if (null? data) '() data)))


(define (current-date-format)
  (let ([date (current-date)])
    (string-append
     (->string (date-year date)) "-"
     (format "~2,'0d" (date-month date)) "-"
     (format "~2,'0d" (date-day date))
     " "
     (format "~2,'0d" (date-hour date))
     ":"
     (format "~2,'0d" (date-minute date))
     ":"
     (format "~2,'0d" (date-second date)))))




(retry-on-error
 (lambda ()
   (string->json
    (simple-get "https://www.dfsdfgg.com" '()))))


(make-error)

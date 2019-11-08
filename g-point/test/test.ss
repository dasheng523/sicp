(library-directories "/Users/huangyesheng/Documents/project/main-world/g-point")
(library-extensions ".ss")
(compile-imported-libraries #t)
(library-requirements-options invoke)

(case (machine-type)
  [(i3le ti3le a6le ta6le) (load-shared-object "libcurl.so.6")]
  [(i3osx ti3osx a6osx ta6osx) (load-shared-object "libcurl.dylib")]
  [else (load-shared-object "libcurl.so")])



(load-shared-object "libc.dylib")
(define getenv1
  (foreign-procedure "getenv" (string) string))

(display (getenv1 "HOME"))



(import (lib curl))


(define curl (curl-easy-init))

(curl-easy-cleanup curl)

(define rss "")

(define callback
  (let* ([x (foreign-callable
             (lambda (content size nmemb _)
               (set! rss
                 (string-append
                  rss
                  (bytevector->string content)))
               (* size nmemb))
             (u8* size_t size_t scheme-object)
             size_t)])
    (lock-object x)
    (foreign-callable-entry-point x)))


(curl-easy-setopt/function
 curl
 CURLOPT_WRITEFUNCTION
 callback)

(let ([url "https://www.baidu.com"])
  (curl-easy-setopt/string curl CURLOPT_URL url)
  (curl-easy-perform curl))



(import (lib curler))
(simple-get "https://www.baidu.com" '())


(import (ffi mysql))

(load-shared-object "libmysqlclient.dylib")

(mysql-get-client-version)

(foreign-entry? "mysql-get-client-version")

(define my-net-init (foreign-procedure "my-net-init" (void* void*) int))



(define (string-replace! old new str)
  (let loop ([len (- (string-length str) 1)] [i 0])
    (cond
     [(< i len)
      (begin
        (if (eq? (string-ref str i) old)
            (string-set! str i new)
            str))
      (loop len (+ i 1))]
     [(>= i len) str])))


(string-replace! "a" "qq" "apple")

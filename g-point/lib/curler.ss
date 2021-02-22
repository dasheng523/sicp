;;; 封装一层 curl，用于构建互联网访问器。

(library (lib curler)
  (export make-curler! curler-close! close-all-curler! curler->visitor
          curler-wrap-headers curler-wrap-cookies curler-wrap-upload
          curler-wrap-get curler-wrap-post-body curler-wrap-post-field
          simple-get simple-post simple-post-body
          )
  (import (chezscheme)
          (ffi curl)
          (lib common))

  ;; 将 '((a . 1) (b .2)) 转换为 a=1&b=2
  (define (map->reqstr data)
    (string-join
     (map
      (lambda (p)
        (string-append
         (->string (car p))
         "="
         (->string (cdr p))))
      data)
     "&"))


  ;; 当前所有curler的列表
  (define all-curler '())


  ;; 创建一个简单的curler
  (define (make-curler!)
    (let* ([curl (curl-easy-init)]
           [result ""]
           [x (foreign-callable
               (lambda (content size nmemb _)
                 (set! result
                   (string-append
                    result
                    (bytevector->string content (native-transcoder))))
                 (* size nmemb))
               (u8* size_t size_t scheme-object)
               size_t)])
      (set! all-curler (cons curl all-curler))
      (cons
       curl
       (lambda (url _)
         (lock-object x)
         (curl-easy-setopt/function curl CURLOPT_WRITEFUNCTION
                                    (foreign-callable-entry-point x))
         (curl-easy-setopt/string curl CURLOPT_URL url)
         (curl-easy-setopt/long curl CURLOPT_FOLLOWLOCATION 1)
         (curl-easy-setopt/long curl CURLOPT_SSL_VERIFYPEER 0)
         (curl-easy-setopt/long curl CURLOPT_SSL_VERIFYHOST 0)
         (curl-easy-perform curl)
         (unlock-object x)
         result))))

  ;; 用完curler后，必须手动关掉。
  (define (curler-close! curler)
    (curl-easy-cleanup (car curler)))

  ;; 关掉所有curler的实例
  (define (close-all-curler!)
    (unless (null? all-curler)
      (curler-close! (car all-curler))
      (set! all-curler (cdr all-curler))))

  ;; 给curler包装一层headers，使得它可以带headers去访问资源
  (define (curler-wrap-headers curler headers)
    (let ([slist (headers->slist headers)]
          [curl (car curler)]
          [handler (cdr curler)])
      (cons curl
            (lambda (url params)
              (curl-easy-setopt/object curl CURLOPT_HTTPHEADER slist)
              (let ([rs (handler url params)])
                (curl-slist-free-all slist)
                rs)))))

  (define (headers->slist headers)
    (let ([slist (make-ftype-pointer
                  curl_slist
                  (foreign-alloc (ftype-sizeof curl_slist)))])
      (define (s-append headers)
        (unless (null? headers)
          (curl-slist-append
           slist
           (string-append (->string (caar (headers)))
                          ":"
                          (->string (cdar (headers)))))
          (s-append (cdr headers))))
      (s-append headers)
      slist))


  ;; 让curler附带一层cookie
  (define (curler-wrap-cookies curler cookie)
    (curl-easy-setopt/object (car curler) CURLOPT_COOKIELIST cookie)
    curler)

  ;; 让curler成为上传器
  (define (curler-wrap-upload curler)
    (let ([curl (curler 'curl)]
          [handler (cdr curler)])
      (curl-easy-setopt/object curl CURLOPT_UPLOAD 1)
      (cons
       curl
       (lambda (url file)
         (curl-easy-setopt/object curl CURLOPT_READDATA file)
         (curl-easy-setopt/long curl CURLOPT_INFILESIZE_LARGE (file-size file))
         (handler url file)))))

  ;; 让curler能够发送get请求
  (define (curler-wrap-get curler)
    (let* ([curl (car curler)]
           [handler (cdr curler)])
      (cons
       curl
       (lambda (url params)
         (handler
          (if (null? params)
              url
              (string-append url "?" (map->reqstr params)))
          params)))))

  ;; 让curler能够发送post请求，其中参数使用postfields
  (define (curler-wrap-post-field curler)
    (let ([curl (car curler)]
          [handler (cdr curler)])
      (cons
       curl
       (lambda (url params)
         (let ((field-str (map->reqstr params)))
           (curl-easy-setopt/long curl CURLOPT_POST 1)
           (curl-easy-setopt/string curl CURLOPT_POSTFIELDS field-str)
           (handler url params))))))

  ;; 让curler可以发送post请求，用body形式提交数据
  (define (curler-wrap-post-body curler)
    (let ([curl (car curler)]
          [handler (cdr curler)])
      (cons
       curl
       (lambda (url params)
         (curl-easy-setopt/long curl CURLOPT_POST 1)
         (curl-easy-setopt/string curl CURLOPT_POSTFIELDS params)
         (handler url params)))))

  ;; 使得curler成为访问器，访问器是一个函数，参数是url和一个params
  (define (curler->visitor curler)
    (cdr curler))

  ;;eg. (simple-get url params)
  (define simple-get
    (curler->visitor
     (curler-wrap-get
      (make-curler!))))


  ;;eg. (simple-post url params)
  (define simple-post
    (curler->visitor
     (curler-wrap-post-field
      (make-curler!))))


  ;;eg. (simple-post-body url body)
  (define simple-post-body
    (curler->visitor
     (curler-wrap-post-body
      (make-curler!))))

  )

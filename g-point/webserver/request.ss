(library (webserver request)
  (export request-method request-url request-protocol request-body request-headers request-cookies
          request-attributes request-header request-cookie request-attribute
          make-request)
  (import (chezscheme)
          (webserver utils))

  (define make-request
    (lambda (method url protocol headers body attributes)
      (lambda (x)
        (record-case
         x
         [(request-method) () method]
         [(request-url) () url]
         [(request-protocol) () protocol]
         [(request-body) () body]
         [(request-headers) () headers]
         [(request-cookies) () (header-cookies)]
         [(request-attributes) () attributes]
         [(request-header) (k) (headers-get headers k)]
         [(request-cookie) (k) (cookies-get cookies k)]
         [(request-attribute) (k) (attributes-get attributes k)]))))

  (record-method request-method)
  (record-method request-url)
  (record-method request-protocol)
  (record-method request-body)
  (record-method request-headers)
  (record-method request-cookies)
  (record-method request-attributes)
  (record-method request-header (k))
  (record-method request-cookie (k))
  (record-method request-attribute (k))

  ;; test
  ;; (define req (make-request 'm 'u 'p 'h 'b 'a))
  ;; (request-cookie req 'k)

  )

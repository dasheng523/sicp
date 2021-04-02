(library (common utils)
  (export identity
          platform-system
          platform-bit
          platform-threaded?
          current-project-dire
          make-map
          map-get
          map-get-default
          map-get-proc
          match-table)
  (import (chezscheme)
          (common combinator)
          (lib common))


  (define system-table
    (list
     (cons "le" 'linux)
     (cons "osx" 'osx)
     (cons "nt" 'windows)))

  (define bit-table
    (list
     (cons (list "i3" "arm32" "ppc32") 32)
     (cons (list "a6") 64)))


  ;; 创建一个类似java的map的列表， k是symbol才能与map-get配合使用
  (define (make-map k v . kvs)
    (let ([n (length kvs)])
      (assert (even? n))
      (cond
       [(= n 0) (list (cons k v))]
       [else (append (make-map k v)
                     (apply make-map kvs))])))

  ;; 可自定义判断条件
  (define map-get-proc
    (compose
     (lambda (x) (and x (cdr x)))
     assp
     (lambda (f m k)
       (values (lambda (x) (f k x))
               m))))
  ;; (map-get-proc eq? (make-map 'a 1 'b 2) 'a)


  ;; 类似get函数，k只能是symbol
  (define map-get
    ((default-argument 0 eq?) map-get-proc))

  (define map-get-default
    (spread-combine (lambda (x y) (or x y)) map-get identity))

  ;; (map-get (make-map 'a 1 'b 2) 'b)
  ;; (map-get-default (make-map 'a 1 'b 2) 'c 666)



  ;; 类似table类似system-table，如果proc匹配，那就返回table的cdr
  (define (common-match-list lst f)
    (lambda (x)
      (exists ((default-argument 1 x) f) lst)))


  (define match-table
    (lambda (table f)
      (common-match-list table
                         (if-condition
                          (lambda (item platform)
                            (pair? (car item)))
                          (nestedcall
                           (parallel-combine common-match-list
                                             car
                                             (lambda (item)
                                               (lambda (k str)
                                                 (and (f str k)
                                                      (cdr item))))))
                          (lambda (item platform)
                            (and (f platform (car item))
                                 (cdr item)))))))


  ;; (platform-system (->string (machine-type)))
  (define platform-system
    (match-table system-table string-end-with))

  ;; (platform-bit "ppc32le")
  (define platform-bit
    (match-table bit-table string-contain?))

  ;; (platform-threaded? (->string (machine-type)))
  (define platform-threaded?
    ((default-argument 1 "t") string-start-with))

  ;; 当前项目目录
  (define current-project-dire
    (compose caar library-directories))

  )


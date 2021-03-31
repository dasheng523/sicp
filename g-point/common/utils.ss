(library (common utils)
  (export platform-system
          platform-bit
          platform-threaded?
          current-project-dire
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




;; 根据include路径，初始化所有头文件
(define init-include-files
  (lambda (include-paths)
    (define find-h-files
      (lambda (path)
        (define h-files?
          (lambda (name)
            (equal? (path-extension name) "h")))
        (define find-files
          (lambda (path)
            (define new-path
              (lambda (file)
                (string-append path (->string (directory-separator)) file)))
            (let loop ([files (directory-list path)]
                       [rs '()])
              (if (null? files)
                  rs
                  (loop (cdr files)
                        (cond [(file-directory? (new-path (car files)))
                               (append rs (find-files
                                           (new-path (car files))))]
                              [(h-files? (car files))
                               (append rs (list (new-path (car files))))]
                              [else rs]))))))
        (find-files path)))

    (define map-h-files
      (lambda (files)
        (let ([ht (make-eq-hashtable)])
          (let loop ([files files])
            (if (null? files)
                ht
                (let ([name (string->symbol (path-last (car files)))])
                  (hashtable-set! ht name (car files))
                  (loop (cdr files))))))))

    (map-h-files (mapcat
                  find-h-files
                  include-paths))))

;; 所有include-paths
(define include-paths
  '("/Users/huangyesheng/Documents/project/main-world"
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.15.sdk/usr/include"))

;; 所有的头文件
(define all-h-files
  (init-include-files include-paths))

;; 通过名字寻找头文件
(define find-h-file-by-name
  (lambda (name)
    (hashtable-ref all-h-files (string->symbol name) #f)))


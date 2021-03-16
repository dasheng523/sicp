(library (lib mysql)
  (export conn-db close-db query-db query-db-1 execute-db)
  (import (chezscheme)
          (ffi mysql))



  ;; 求值一组表达式，返回结果之前，会调用finish。
  ;; 一般用于做close，free，或者做结果验证之类操作。
  (define-syntax with-finish
    (syntax-rules ()
      [(_ finish ([name e] ...) expr ...)
       (let ([name e] ... )
         (let ([rs (begin expr ...)])
           (finish name) ...
           rs))]
      [(_ finish expr ...)
       (let ([rs (begin expr ...)])
         (finish rs)
         rs)]))


  (define (conn-db host user passwd db port)
    (let ([conn (mysql-init 0)])
      (if (null? conn)
          (raise "数据库连接异常")
          (with-finish
           (lambda (rs) (if (ftype-pointer-null? rs)
                            (error 'conn-db (mysql-error conn))))
           (mysql-real-connect conn host user passwd db port #f 0)))))


  (define (close-db conn)
    (mysql-close conn))


  ;; 将char*类型的指针转化为字符串
  (define (char*->string address)
    (utf8->string
     (apply bytevector
            (let iter ([i 0])
              (let ((bit (foreign-ref 'unsigned-8 address i)))
                (if (= bit 0)
                    '()
                    (cons bit (iter (+ i 1)))))))))

  ;; 将字符串转化为char*指针，记得free
  (define (string->char* s)
    (let* ([slen (string-length s)]
           [char* (foreign-alloc (* slen 2))]
           [ip (open-bytevector-input-port (string->utf8 s))])
      (let iter ([i 0])
        (let ([bit (get-u8 ip)])
          (if (eof-object? bit)
              (begin (foreign-set! 'unsigned-8 char* i 0)
                     char*)
              (begin
                (foreign-set! 'unsigned-8 char* i bit)
                (iter (+ 1 i))))))))


  ;; 转码,用于避免SQL注入
  #;(define (escape-string s)
    (with-finish
     (lambda (x) (foreign-free (ftype-pointer-address x)))
     ([charpoint (make-ftype-pointer char (foreign-alloc 1))])
     (mysql-real-escape-string conn charpoint s (string-length s))
     (char*->string (ftype-pointer-address charpoint))))


  (define (row-index row i)
    (let ([fpointer (ftype-ref MYSQL_ROW (i) row)])
      (if (ftype-pointer-null? fpointer)
          (void)
          (char*->string
           (ftype-pointer-address fpointer)))))


  (define (get-field-name fields i)
    (char*->string
     (ftype-pointer-address
      (ftype-ref MYSQL_FIELDS (i name) fields))))

  (define (compose-data fields-num fields row)
    (let iter ([i 0]
               [rs '()])
      (if (>= i fields-num)
          rs
          (iter (+ i 1)
                (append
                 rs
                 (list (cons (get-field-name fields i)
                             (row-index row i))))))))

  ;; 查询数据库，返回多条记录
  (define (query-db conn sql)
    (with-finish
     (lambda (x) (if (not (= 0 x)) (error 'query-db (mysql-error conn))))
     (mysql-query conn sql))
    (with-finish
     mysql-free-result
     ([resultset (mysql-store-result conn)])
     (let* ([fields-num (mysql-num-fields resultset)]
            [fields (mysql-fetch-fields resultset)])
       (let iter ()
         (let ([row (mysql-fetch-row resultset)])
           (if (ftype-pointer-null? row)
               '()
               (cons (compose-data fields-num fields row)
                     (iter))))))))

  ;; 查询数据库，返回一条记录
  (define (query-db-1 conn sql)
    (with-finish
     (lambda (x) (if (not (= 0 x)) (error 'query-db (mysql-error conn))))
     (mysql-query conn sql))
    (with-finish
     mysql-free-result
     ([resultset (mysql-store-result conn)])
     (let* ([fields-num (mysql-num-fields resultset)]
            [fields (mysql-fetch-fields resultset)]
            [row (mysql-fetch-row resultset)])
       (compose-data fields-num fields row))))

  ;; 把sql传给db执行
  (define (execute-db conn sql)
    (with-finish
     (lambda (x) (if (not (= 0 x)) (error 'query-db (mysql-error conn))))
     (mysql-query conn sql)))




  #| test
  (define conn (conn-db "localhost" "root" "a5235013" "cgkc" 3306))
  (mysql-query conn "select * from adviser limit 10")
  (define result (mysql-store-result conn))
  (define aa (mysql-fetch-row result))
  (define fields-num (mysql-num-fields result))

  (define fields (mysql-fetch-fields result))

  (compose-data fields-num fields aa)

  (query-db conn "select 1 as d")

  (escape-string "' OR ''='")

  (execute-db conn "update adviser set name='888' where id=4")
  |#

  )

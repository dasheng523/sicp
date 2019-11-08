#lang racket
(require racket/class)
(require db)


(define mydb (sqlite3-connect #:database "blog" #:mode 'create))

(define (init-db)
  (unless (table-exists? mydb "user")
    (query-exec mydb
     "CREATE TABLE user (id INTEGER, name VARCHAR(30), password VARCHAR(30))")
    (query-exec mydb
     "insert into user values (1, '523', '8888')"))

  (unless (table-exists? mydb "article")
    (query-exec mydb
     "CREATE TABLE article (id INTEGER, title VARCHAR(30), content TEXT, author INTEGER)")))

(init-db)


;;将数据库的返回结果转换为hash的list
(define (convert-to-list query-result)
  (define (iter result header row)
    (cond [(null? header) result]
          [else (iter (cons (cons (hash-ref (make-hash (car header)) 'name) (car row)) result)
                      (cdr header)
                      (cdr row))]))
  (define (iter-rows result header rows)
    (if (null? rows)
        result
        (iter-rows (append result
                           (list (make-hash
                                  (iter '() header (vector->list (car rows))))))
                   header
                   (cdr rows))))
  (iter-rows '() (rows-result-headers query-result) (rows-result-rows query-result)))


(define (db-finds sql . args)
  (convert-to-list
   (apply query mydb sql args)))




;; update-site的参数是一个hashtable，到底对不对呢？
;; 不对，因为上层需要知道data的细节。但是使用site就很不错了。
;; 那约定界面是什么？我设想是做一个entity界面，所有的entity都能直接保存到数据库中。
;; 他们能进行组合，变成一个能连接大量entity的对象。
(define entity-layout%
  (class object%
    (super-new)
    (init-field table fields relations)
    (define/public (get-table)
      table)
    (define/public (get-fields)
      fields)
    (define/public (get-relations)
      relations)))



(define layout-relation%
  (class object%
    (super-new)
    (init-field name target-table target-field main-field)
    (define/public (get-name)
      name)
    (define/public (get-target-table)
      target-table)
    (define/public (get-target-field)
      target-field)
    (define/public (get-main-field)
      main-field)))

;; 连接两个layout，使之变成一个
(define (relate-layout relation-name
                       master-layout
                       target-layout
                       main-field
                       target-field)
  (new entity-layout%
       [table (send master-layout get-table)]
       [fields (send master-layout get-fields)]
       [relations (cons (new layout-relation%
                             [name relation-name]
                             [target-table (send target-layout get-table)]
                             [target-field target-field]
                             [main-field main-field])
                        (send master-layout get-relations))]))


(define entity%
  (class object%
    (super-new)
    (init-field layout data)
    
    (define inner-data
      (make-hash
       (foldl (lambda(k ls)
                (cons (cons k (hash-ref data k)) ls))
              '()
              (send layout get-fields))))

    (define (make-relation-data relation)
      (define sql (string-append "select * from "
                                 (send relation get-target-table)
                                 " where "
                                 (send relation get-target-field)
                                 " = ?"))
      (cons
       (send relation get-name)
       (lambda ()
         (map (lambda (item)
                (new entity%
                     [layout (send relation get-target-table)]
                     [data item]))
              (db-finds sql (get-val (send relation get-main-field)))))))

    
    (define relation-data
      (make-hash
       (map make-relation-data
            (send layout get-relations))))

    (define/public (get-val field)
      (cond [(hash-has-key? inner-data field)
             (hash-ref inner-data field)]
            [(hash-has-key? relation-data field)
             ((hash-ref relation-data field))]
            [else #f]))

    (define/public (set-val field data)
      (hash-set! inner-data field data))))




(define layout-user (new entity-layout%
                     [table "user"]
                     [fields (list "id" "name" "password")]
                     [relations '()]))

(define layout-article (new entity-layout%
                     [table "article"]
                     [fields (list "id" "title" "content" "author")]
                     [relations '()]))


(define layout-relate-user (relate-layout "articles" layout-user layout-article "id" "author"))

(define user-entity (new entity%
                         [layout layout-relate-user]
                         [data (hash "id" 1 "name" "523" "password" "888888")]))

(send user-entity set-val "name" 8899)
(send user-entity get-val "name")




(define aquery
  (make-query
   (table 'user)
   (where 'name '= "523")
   (orderby 'id 'desc)
   (select 'id 'name 'passwd)
   (limit 0 10)))

(to-sql aquery)

(query-db aquery)

(define (make-query . exprs)
  (cond [(null? exprs) (make-query (select 1))]
        [else (vector-sort (list->vector exprs)
                           (lambda (a b)
                             (< (hash-ref expr-ordermap (expr-type a))
                                (hash-ref expr-ordermap (expr-type b)))))]))

(define (to-sql query)
  (define (handle exprslist)
    (let [(expr (car exprslist))]
      (append (expr->sql expr)
              (handle (cdr exprslist)))))
  (handle (vector->list query)))

(define (expr->sql expr)
  )

(define(expr-keyword expr)
  (string-replace
   (symbol->string (expr-keyword expr))
   "-"
   " "))


(define-generics expr
    (tosql expr))

(struct table (keyword name)
  	#:methods gen:expr
  )


(define (table name)
  (expr "from" name))

(define (is-table? expr)
  (eq? "from" (expr-keyword)))

(define (where . args)
  (expr "where" args))

(define (is-where? expr)
  (eq? "where" (expr-keyword)))

(define (orderby . args)
  (expr "order by" args))

(define (is-orderby? expr)
  (eq? "order by" (expr-keyword)))

(define (select . args)
  (expr "select" args))

(define (is-select? expr)
  (eq? "select" (expr-keyword)))

(define (limit . args)
  (expr "limit" args))

(define (is-limit? expr)
  (eq? "limit" (expr-keyword)))

(define (groupby . args)
  (expr "group by" args))

(define (is-groupby? expr)
  (eq? "groupby" (expr-keyword)))

















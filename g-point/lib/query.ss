(library (lib query)
  (import (chezscheme)
          (lib common))
  (export query->sql make-query query-compose
          where-or where-and where
          join left-join right-join outer-join
          from group-by select order-by)

  (define-ftype AA
    (struct [aa string]))
  (array 100 string)

  ;; 如果表达式不用符号，而是用字符串会怎么样？
  ;; (where "name='666'") 这种情况，如果想要参数传进去，就需要使用字符串替换了。
  ;; 而表达式只需要反解析就行了 (where `(name = ,name))
  ;; 还有一个场景是，前端有两个选项，1显示热门文章，2显示最新文章。这两个选项的区别是排序以及热门文章需要显示热度，最新文章不显示热度。从sql上来说，两个sql不同之处在于order by，以及select的某个字段，其他部分都一样。如果想把通用部分抽离出来，字符串形式就不是很好用了。如果硬要使用字符串形式，那么就需要借助模板语言才能完成。而将Sql结构化之后，就不需要那么麻烦。
  ;; 使用这种结构化也有弊端，比如通用模板需要改动的时候，会影响所有依赖这个模板的其他模板。想了想，没这个弊端。
  ;; 有机会再写文章说明这个问题吧。
  #;(query->sql
   (query-compose
    (make-query
     (where-or `(name = "adf")
               '(sex = 1)
               (where-and '(name = "ddd")
                          '(email = "qqq@qqq.comx")))
     (where-and '(name = "544"))
     (from '(user u))
     (group-by '[name])
     (left-join '[user u on account.user_id = u.id])
     (select '[name id "min(score) as ms"])
     (select '[sex]))
    (make-query
     (order-by '[name desc])
     (left-join '[address on address.id = user.address_id])
     )))

  (define (make-query . exprs)
    (map
     (lambda (es)
       (if (null? (cdr es))
           (car es)
           (apply expr-merge es)))
     (exprs-patition exprs)))



  (define(query-subexprs query)
    query)

  (define (query-compose . queries)
    (apply make-query
           (apply append
                  (map query-subexprs queries))))

  (define (query->sql query)
    (when (null? (exprs-contain? (query-subexprs query) 'from))
      (raise "必须包含from语句"))
    (let ([query (if (null? (exprs-contain? (query-subexprs query) 'select))
                     (query-compose query
                                    (make-query (select '*)))
                     query)])
      (let iter ([exprs (exprs-sort (query-subexprs query))])
        (if (null? exprs)
            ""
            (string-append (expr->sql (car exprs))
                           " "
                           (iter (cdr exprs)))))))

  ;; 对表达式进行分组
  (define (exprs-patition exprs)
    (let iter ([rs '()] (exprs exprs))
      (if (null? exprs)
          rs
          (let-values ([(group left) (partition (lambda (x)
                                 (eq? (expr-type (car exprs))
                                      (expr-type x)))
                               exprs)])
            (iter (cons group rs)
                  left)))))


  (define (exprs-contain? exprs type)
    (define (find-expr exprs)
      (cond
       [(null? exprs) #f]
       [(eq? type (expr-type (car exprs))) #t]
       [else (find-expr (cdr exprs))]))
    (find-expr exprs))

  (define (exprs-find exprs type)
    (filter
     (lambda (x) (eq? type (expr->type x)))
     (exprs-find exprs)))

  (define (exprs-sort exprs)
    (list-sort
     (lambda (e1 e2)
       (< (expr-sort-weight e1) (expr-sort-weight e2)))
     exprs))

  (define (expr->sql expr)
    ((expr '->sql)))

  (define (expr-sort-weight expr)
    ((expr 'sort-weight)))

  (define (expr-type expr)
    ((expr 'type)))

  (define (expr-merge . els)
    (define (iter rs els)
      (cond ((null? els) rs)
            (else (let ([item (car els)])
                    ((item 'merge) rs item)))))
    (iter (car els) (cdr els)))


  ;;;; where
  (define (where-generator delim)
    (lambda (els)
      (define (->sql)
        (string-append
         "where "
         (params->sql)))

      (define (params->sql)
        (string-join
         (map (lambda (e)
                (cond [(pair? e)
                       (string-append
                        (symbol->string (car e))
                        (symbol->string (cadr e))
                        (escape-string (caddr e)))]
                      [else
                       (string-append "(" ((e 'params->sql)) ")")]))
              els)
         (string-append " " delim " ")))

      (define (type) 'where)

      (define (sort-weight) 4)

      (define (merge e1 e2)
        ((where-generator "and") (list e1 e2)))

      (lambda (op)
        (cond [(eq? op '->sql) ->sql]
              [(eq? op 'type) type]
              [(eq? op 'merge) merge]
              [(eq? op 'params->sql) params->sql]
              [(eq? op 'sort-weight) sort-weight]
              [else (raise "操作不存在")]))))


  (define (escape-string x)
    (if (string? x)
        (string-append "'" x "'")
        (->string x)))

  (define (where-and . els)
    ((where-generator "and") els))

  (define (where-or . els)
    ((where-generator "or") els))

  (define where
    where-and)


;;;;; select
  (define (select . expr)
    (define (->sql)
      (string-append
       "select "
       (string-join
        (map ->string (apply append expr))
        ", ")))

    (define (type) 'select)

    (define (merge e1 e2)
      (select (append ((e1 'get-expr)) ((e2 'get-expr)))))

    (define (get-expr) (apply append expr))

    (define (sort-weight) 1)

    (lambda (op)
      (cond [(eq? op '->sql) ->sql]
            [(eq? op 'type) type]
            [(eq? op 'merge) merge]
            [(eq? op 'get-expr) get-expr]
            [(eq? op 'sort-weight) sort-weight]
            [else (raise "操作不存在")])))


  ;;;; from
  (define (from expr)
    (define (->sql)
      (string-append
       "from "
       (string-join
        (map ->string expr)
        " ")))

    (define (type) 'from)

    (define (sort-weight) 2)

    (define (merge e1 e2)
      (raise "不能合并多个from"))

    (lambda (op)
      (cond [(eq? op '->sql) ->sql]
            [(eq? op 'type) type]
            [(eq? op 'merge) merge]
            [(eq? op 'sort-weight) sort-weight]
            [else (raise "操作不存在")])))


  ;;;;;; order by
  (define (order-by . expr)
    (define (->sql)
      (string-append
       "order by "
       (string-join
        (map
         (lambda (x)
           (string-append (->string (car x))
                          " "
                          (->string (cadr x))))
         expr)
        ", ")))

    (define (type) 'order-by)

    (define (sort-weight) 8)

    (define (merge e1 e2)
      (apply order-by
             (append ((e1 'get-expr))
                     ((e2 'get-expr)))))

    (define (get-expr)
      expr)

    (lambda (op)
      (cond [(eq? op '->sql) ->sql]
            [(eq? op 'type) type]
            [(eq? op 'merge) merge]
            [(eq? op 'get-expr) get-expr]
            [(eq? op 'sort-weight) sort-weight]
            [else (raise "操作不存在")])))


 ;;;;;;;;;; join
  (define (common-join . expr)
    (define (->sql)
      (string-join
       (map (lambda (e)
              (let ([ls (map ->string e)])
                (string-append
                 (car ls)
                 " join "
                 (string-join (cdr ls) " "))))
            expr)
       " "))

    (define (type) 'join)

    (define (sort-weight) 3)

    (define (merge e1 e2)
      (apply common-join (append ((e1 'get-expr)) ((e2 'get-expr)))))

    (define (get-expr)
      expr)

    (lambda (op)
      (cond [(eq? op '->sql) ->sql]
            [(eq? op 'type) type]
            [(eq? op 'merge) merge]
            [(eq? op 'get-expr) get-expr]
            [(eq? op 'sort-weight) sort-weight]
            [else (raise "操作不存在")])))

  (define (join . exprs)
    (apply common-join (map
                  (lambda (x) (cons 'inner x))
                  exprs)))


  (define (left-join . exprs)
    (apply common-join (map
                        (lambda (x) (cons 'left x))
                        exprs)))

  (define (right-join . exprs)
    (apply common-join (map
                        (lambda (x) (cons 'right x))
                        exprs)))

  (define (outer-join . exprs)
    (apply common-join (map
                        (lambda (x) (cons 'outer x))
                        exprs)))


  ;;;;;; group by
  (define (group-by . exprs)
    (define (->sql)
      (string-append "group by "
                     (string-join
                      (map ->string (apply append exprs))
                      ", ")))

    (define (type) 'group-by)

    (define (sort-weight) 7)

    (define (merge e1 e2)
      (apply group-by (append ((e1 'get-expr)) ((e2 'get-expr)))))

    (define (get-expr)
      exprs)

    (lambda (op)
      (cond [(eq? op '->sql) ->sql]
            [(eq? op 'type) type]
            [(eq? op 'merge) merge]
            [(eq? op 'get-expr) get-expr]
            [(eq? op 'sort-weight) sort-weight]
            [else (raise "操作不存在")])))



  )

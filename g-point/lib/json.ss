(library (lib json)
  (export
    string->json
    json->string
  )
  (import (lib common)
          (common file)
          (common combinator))

  (define (make-pattern check)
    (cons check #f))

  (define (pattern-check pattern)
    (car pattern))


  (define (parse pattern str)
    (and (> (string-length str) 0)
         ((pattern-check pattern) str)))

  ;; 跳过模式前面的空白字符
  (define (filter-blank-pattern p)
    (make-pattern
     (lambda (s)
       (let loop ([i 0] [s s])
         (let ([len (string-length s)])
           (cond
            [(and (> len 0) (char-whitespace? (string-ref s 0)))
             (loop (+ i 1) (substring s 1 len))]
            [else (let ([rs (parse p s)])
                    (and rs
                         (cons (car rs) (+ i (cdr rs)))))]))))))

  (assert (equal? (parse (filter-blank-pattern (make-simple-pattern "a")) "   a")
                  (cons "a" 4)))


  (define (make-simple-pattern s)
    (make-pattern (lambda (k)
                    (and (string-start-with k s)
                         (cons s (string-length s))))))
  ;; (parse (make-simple-pattern "{") "{88")




  ;; 如果符合模式p, 则匹配第一个字符，否则#f
  ;; (parse (first-char-pattern (make-simple-pattern "abbb")) "abbbbbb")
  (define (first-char-pattern p)
    (define (do-match k)
      (let ([rs (parse p k)])
        (and rs (cons (string-ref k 0) 1))))
    (make-pattern do-match))

  ;; 模式取否，如果匹配#f
  ;; (parse (not-pattern (make-simple-pattern "abbb")) "babbbbbb")
  (define (not-pattern p)
    (define (do-match k)
      (let ([rs (parse p k)])
        (and (not rs)
             (cons k 0))))
    (make-pattern do-match))


  ;; or 组合
  (define (or-pattern p . rest)
    (define (do-compose ps)
      (lambda (k)
        (let loop ([ps ps])
          (cond
           [(null? ps) #f]
           [else (or (parse (car ps) k)
                     (loop (cdr ps)))]))))
    (make-pattern (do-compose (cons p rest))))

  (assert (equal? (parse (or-pattern (make-simple-pattern "a")
                                     (make-simple-pattern "666")) "666")
                  (cons "666" 3)))

  ;;(parse number-pattern "a666")


  (define empty-pattern
    (make-pattern
     (lambda (k)
       (cons '() 0))))

  (assert (equal? (parse empty-pattern "aa")
                  (cons '() 0)))


  ;; and 组合
  (define (and-pattern pattern . rest)
    (define (the-compose ps)
      (lambda (k)
        (let loop ([rs '()] [i 0] [k k] [ps ps])
          (cond
           [(string=? k "") (cons rs i)]
           [(null? ps) (cons rs i)]
           [else (let ([pd (parse (car ps) k)]
                       [len (string-length k)])
                   (and pd
                        (cond
                         [(and (= len (cdr pd)) (null? (cdr ps)))
                          (cons (append rs (list (car pd)))
                                (+ i (cdr pd)))]
                         [(> len (cdr pd))
                          (loop (append rs (list (car pd)))
                                (+ i (cdr pd))
                                (substring k (cdr pd) len)
                                (cdr ps))]
                         [else #f])))]))))
    (make-pattern (the-compose (cons pattern rest))))

  (assert (equal? (parse (and-pattern number-pattern (make-simple-pattern "b")) "12b")
                  (cons '(12 "b") 3)))
  (assert (not (parse (and-pattern number-pattern (make-simple-pattern "b")) "1")))


  ;; 匹配n次
  (define (repeat-pattern p n)
    (apply and-pattern (make-list n p)))


  ;; 一直匹配，直到匹配失败
  (define (while-pattern p)
    (define (the-compose k)
      (let loop ([rs '()] [i 0] [k k])
        (let ([pd (parse p k)]
              [len (string-length k)])
          (cond
           [(and pd (= len (cdr pd)))
            (cons (append rs (list (car pd)))
                  (+ i (cdr pd)))]
           [(and pd (> len (cdr pd)))
            (loop (append rs (list (car pd)))
                  (+ i (cdr pd))
                  (substring k (cdr pd) len))]
           [else (cons rs i)]))))
    (make-pattern the-compose))

  (assert (equal? (parse (while-pattern (filter-blank-pattern (make-simple-pattern "a"))) "a a a")
                  (cons '("a" "a" "a") 5)))


  ;; 将模式的匹配值转换一下
  (define (convert-pattern p f)
    (make-pattern
     (lambda (s)
       (let ([rs (parse p s)])
         (and rs
              (cons (f (car rs))
                    (cdr rs)))))))


  ;; join模式，类似string-join
  (define (join-pattern p slim)
    (convert-pattern
     (and-pattern
      (while-pattern (and-pattern p slim))
      p)
     (lambda (x)
       (append
        (map (lambda (item) (car item)) (car x))
        (cdr x)))))

  (assert (equal? (parse (join-pattern
                     (repeat-pattern (make-simple-pattern "a") 3)
                     (make-simple-pattern "b"))
                    "aaabaaa")
                  (cons '(("a" "a" "a") ("a" "a" "a")) 7)))

  (assert (equal? (parse (join-pattern
                          (filter-blank-pattern (make-simple-pattern "a"))
                          (filter-blank-pattern (make-simple-pattern ",")))
                         " a , a")
                  (cons '("a" "a") 6)))



  (define (filter-empty-pattern p)
    (make-pattern
     (lambda (s)
       (let ([rs (parse p s)])
         (if (null? (car rs))
             #f
             rs)))))
  (assert (not (parse (filter-empty-pattern (while-pattern (make-simple-pattern "a"))) "qq")))

  ;; 布尔模式
  (define boolean-pattern
    (convert-pattern
     (apply or-pattern (map make-simple-pattern (list "true" "false")))
     (lambda (x)
       (string=? x "true"))))

  (assert (equal? (parse boolean-pattern "true")
                  (cons #t 4)))
  (assert (equal? (parse boolean-pattern "false")
                  (cons #f 5)))

  ;; null模式
  (define null-pattern
    (convert-pattern
     (make-simple-pattern "null")
     (lambda (x) #f)))

  (assert (equal? (parse null-pattern "null")
                  (cons #f 4)))


  ;; 数字模式
  (define number-pattern
    (let ([lst (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")])
      (convert-pattern
       (filter-empty-pattern
        (while-pattern (apply or-pattern (map make-simple-pattern lst))))
       (lambda (x)
         (if (null? x)
             #f
             (string->number (string-join x "")))))))

  (assert (equal? (parse number-pattern "905a") (cons 905 3)))
  (assert (eq? (parse number-pattern "a905a") #f))
  (assert (equal? (parse (join-pattern number-pattern (make-simple-pattern ",")) "1,2,3,4")
                  (cons '(1 2 3 4) 7)))




  ;; 字符串模式
  (define string-pattern
    (convert-pattern
     (and-pattern (make-simple-pattern "\"")
                  (or-pattern
                   (filter-empty-pattern
                    (while-pattern (or-pattern
                                    (convert-pattern
                                     (and-pattern (make-simple-pattern "\\") (make-simple-pattern "\""))
                                     (lambda (x) #\"))
                                    (first-char-pattern (not-pattern (make-simple-pattern "\""))))))
                   (convert-pattern empty-pattern
                                    (lambda (x) '())))
                  (make-simple-pattern "\""))
     (lambda (x)
       (list->string (cadr x)))))

  (assert (equal? (parse string-pattern "\"dsfas\\\"df\"asd")
                  (cons "dsfas\"df" 11)))
  (assert (equal? (parse string-pattern "\"\"")
                  (cons "" 2)))


  ;; 懒模式
  (define (lazy-pattern f)
    (make-pattern
     (lambda (k)
       (parse (f) k))))

  ;; (parse (lazy-pattern (lambda () number-pattern)) "3344")

  (define skip-blank-simple-pattern
    (compose filter-blank-pattern make-simple-pattern))

  (define json-pattern
    (letrec ([make-json-pattern
              (lambda ()
                (let ([lst (list (lazy-pattern make-array-pattern)
                                 (lazy-pattern make-object-pattern)
                                 number-pattern
                                 string-pattern
                                 boolean-pattern
                                 null-pattern)])
                  (apply or-pattern (map filter-blank-pattern lst))))]
             [make-array-pattern
              (lambda ()
                (convert-pattern
                 (and-pattern
                  (skip-blank-simple-pattern "[")
                  (or-pattern
                   (join-pattern (lazy-pattern make-json-pattern)
                                 (skip-blank-simple-pattern ","))
                   empty-pattern)
                  (skip-blank-simple-pattern "]"))
                 (lambda (x)
                   (cadr x))))]
             [make-object-pattern
              (lambda ()
                (convert-pattern
                 (and-pattern
                  (skip-blank-simple-pattern "{")
                  (join-pattern (and-pattern
                                 (filter-blank-pattern string-pattern)
                                 (skip-blank-simple-pattern ":")
                                 (lazy-pattern make-json-pattern))
                                (skip-blank-simple-pattern ","))
                  (skip-blank-simple-pattern "}"))
                 (lambda (x)
                   (map (lambda (x) (cons (car x) (caddr x))) (cadr x)))))])
      (make-json-pattern)))

  (assert (equal? (parse json-pattern "[1,2,\"fff\",4]")
                  (cons '(1 2 "fff" 4) 13)))

  (assert (equal? (parse json-pattern "[]")
                  (cons '() 2)))
  (assert (equal? (parse json-pattern "[1,\"a\",[1,2],4]")
                  (cons '(1 "a" (1 2) 4) 15)))

  (assert (equal? (parse json-pattern " [1,2 ,\"f ff\", 4]")
                  (cons '(1 2 "f ff" 4) 17)))

  (assert (equal? (parse json-pattern " null")
                  (cons #f 5)))

  (assert (equal? (parse json-pattern " \"\"")
                  (cons "" 3)))


  (assert (equal? (parse json-pattern " {\"a\" : [2, 7],\"b\" :\"6 \"\t ,\"c\"\n : false}")
                  (cons (list (cons "a" (list 2 7))
                              (cons "b" "6 ")
                              (cons "c" #f))
                        40)))


  (define (string->json s)
    (let ([rs (parse json-pattern s)])
      (if rs
          (car rs)
          (error 'string->json "json格式不正确"))))


  ;; (read-string-with-file "e:/aaa.txt" string->json)
)

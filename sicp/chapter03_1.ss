;; 3.1 赋值和局部状态

;; 3.1.1 局部状态变量
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 10)


(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(new-withdraw 10)


(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 50))
(define W2 (make-withdraw 70))

(W1 40)
(W2 40)


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))

  dispatch)



;; 练习 3.1
(define (make-accumulator init)
  (define (add num)
    (set! init (+ init num))
    init)

  add)

(define A (make-accumulator 5))

(A 10)

(A 10)


;; 练习 3.2
(define (make-monitored fn)
  (let ((amount 0))
    (define (dispatch op)
      (cond ((eq? op 'how-many-calls?) amount)
            ((eq? op 'reset-count) (set! amount 0))
            (else (begin
                    (set! amount (+ amount 1))
                    (fn op)))))
    dispatch))

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)
(s 'reset-count)



;; 练习 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch pass m)
    (if (eq? pass password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (lambda (_) (error "Incorrect password"))))

  dispatch)


(define acc (make-account 100 'abc))

((acc 'abc 'withdraw) 40)
((acc 'eee 'withdraw) 20)


;; 练习 3.4
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (call-the-cops)
    (display "call-the-cops"))

  (define (warn-creator)
    (let ((count 0))
      (define (warn amount)
        (if (< count 7)
            (begin
              (set! count (+ count 1))
              "Incorrect password")
            (call-the-cops)))
      warn))

  (let ((count 0))
    (define (dispatch pass m)
      (if (eq? pass password)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT" m)))
          (begin
            (if (< count 7)
                (set! count (+ count 1))
                (call-the-cops))
            (lambda (_) "Incorrect password"))))

    dispatch))


(define acc (make-account 100 'abc))

((acc 'aaa 'withdraw) 10)



;; 3.1.2 引进赋值带来的利益
(define (rand)
  (let ((x random-init))
    (lambda ()
      (set! x (random-update x))
      x)))


(define (rand)
  (random 1000000000))

(rand)

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))


(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (random-update x))
          (x2 (random-update x2)))
      (cond ((= trials-remaining 0)
             (/ trials-passed trials))
            ((= (gcd x1 x2) 1)
             (iter (- trials-remaining 1)
                   (+ trials-passed 1)
                   x2))
            (else
             (iter (- trials-remaining 1)
                   trials-passed
                   x2))))))


;; 练习 3.5
(define (estimate-intergral p x1 x2 y1 y2 trials)
  (* (monte-carlo trials p)
     (abs (* (- x2 x1)
             (- y2 y1)))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


(define (square x)
  (* x x))

(define (estimate-pi trials)
  (let ((x1 2)
        (x2 8)
        (y1 4)
        (y2 10)
        (round-x 5)
        (round-y 7)
        (round-radius 3))
    (define (p-test)
      (<= (+ (square (- (random-in-range x1 x2) round-x))
             (square (- (random-in-range y1 y2) round-y)))
          (square round-radius)))
    (/ (* (estimate-intergral p-test x1 x2 y1 y2 trials)
          1.0)
       (square round-radius))))

(estimate-pi 10000000)


(define (rand op)
  (let ((x (random-init)))
    (cond ((eq? op 'generate)
           (begin (set! x (random-update x))
                  x))
          ((eq? op 'reset)
           (lambda (new-x)
             (set! x new-x))))))



;; 练习 3.7
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (verify-password pass)
    (eq? pass password))

  (define (dispatch pass m)
    (if (verify-password pass)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'verify) #t)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (begin (error "Incorrect password")
               #f)))

  dispatch)

(define (make-joint account password new-password)
  (if (account password 'verify)
      (lambda (pass m)
        (if (eq? pass new-password)
            (account password m)
            (lambda (_) (error "Incorrect password"))))
      (error "Incorrect password")))


(define petter-acc (make-account 100 'abc))

(define paul-acc (make-joint petter-acc 'abc 'aaa))

((paul-acc 'aaa 'withdraw) 10)


;; 练习 3.8
(define f
  (let ((status 1))
    (lambda (x)
      (if (= 0 x) (set! status 0))
      (if (= status 0) 0 x))))

(+ (f 0) (f 1))

;; 返回的是0,也就是chez scheme的求值顺序是从左到右的。



;; 练习 3.12
(define x (list 'a 'b))
(define y (list 'c 'd))

(define z (append x y))

(cdr x) ; (b)

(define (append! x y)
  (set-cdr! (last-pair x) y))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define w (append! x y))

(cdr x) ; (b c d)



;; 练习 3.13
;; 如果执行(last-pair z)会出现死循环情况，因为z没有最后的项了，它是一个闭环。


;; 练习 3.14
;; mystery将x里的元素反转。
;; v是 ('a) w是 ('d 'c 'b 'a)
;; 这是因为第一次调用loop，传给它的参数是x,这时候的x就是v，所以执行指挥，v就被给成了（a）。调用下一次loop的时候，loop中的x被绑定的是(b c d)，也就是(cdr v)，从那个时候起，set-cdr!就不再能影响v了。
;; 使用了赋值以后，参数可能会发生很难预料的改变，所以不能继续依赖以前那个变量了。

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))


;; 练习 3.16
;; 返回3的情况
(define result (cons 1 (cons 2 (cons 3 3))))

;; 返回4的情况
(define a (cons 1 1))
(define result (cons a
                     (cons 2 a)))

;; 返回7的情况
(define a (cons 1 1))
(define b (cons a a))
(define result (cons b b))

;; 不返回的情况,环形结构
(define a (cons 1 b))
(define b (cons 2 c))
(define c (cons 3 a))


;; 练习 3.17
(define (count-pairs x)
  (define (iter x bs)
    (cond ((not (pair? x)) #f)
          (((bs 'visited?) x)
           (begin
             (iter (car x) bs)
             (iter (cdr x) bs)))
          (else
           (begin
             ((bs 'visite) x)
             (iter (car x) bs)
             (iter (cdr x) bs)))))
  (let ((bs (make-blocks)))
    (iter x bs)
    ((bs 'length))))

(define (make-blocks)
  (let ((blocks '()))
    (define (visite x)
      (set! blocks (cons x blocks)))

    (define (visited? x)
      (included? x blocks))

    (define (blocks-length)
      (length blocks))

    (lambda (op)
      (cond ((eq? op 'visite) visite)
            ((eq? op 'visited?) visited?)
            ((eq? op 'length) blocks-length)))))

(define (pairs-eq? x1 x2)
  (and (eq? (car x1) (car x2))
       (eq? (cdr x1) (cdr x2))))

(define (included? x ls)
  (cond ((null? ls) #f)
        ((pairs-eq? x (car ls)) #t)
        (else (included? x (cdr ls)))))



(count-pairs result)



;; 练习 3.18
(define (cycle? x)
  (define (iter x bs)
    (cond ((not (pair? x)) #f)
          (((bs 'visited?) x) #t)
          (else (begin
                  ((bs 'visite) x)
                  (iter (cdr x) bs)))))
  (iter x (make-blocks)))


(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(cycle? (make-cycle (list 'a 'b 'c 'd)))
(cycle? result)


;; 练习 3.19
;; 要判断是否包含一个环，其中最关键的在于确定序列的同一性。
;; 练习3.18中，通过给序列加入到已访问的序列里来确认同一性，而构建一个已访问序列依赖于x的长度，所以空间复杂度是n。
;; 还可以通过另一个方式来确认同一性，那就是直接修改序列。

;; 修改序对
(define (modify x)
  (set-car! x 'modify))


;; 判断序对是否被修改过
(define (is-modify? x)
  (eq? 'modify (car x)))


;; 这过程会对入参进行大破坏。。。但也能用。
(define (cycle?! x)
  (cond ((not (pair? x)) #f)
        ((null? (cdr x)) #f)
        ((is-modify? (cdr x)) #t)
        (else
         (let ((cardata (car x)))
           (modify x)
           (or (cycle?! (cdr x))
               (cycle?! cardata))))))


(cycle?! (make-cycle (list 'a 'b 'c 'd)))
(cycle?! (list 'a 'b 'c 'd))



;; 改变也就是赋值
;; 我不是很明白这句话的含义。我只能从表面上理解想要改变，就需要赋值。而赋值意味着改变。

(define (cons x y)
  (define (set-x! v) (set! x v))

  (define (set-y! v) (set! y v))

  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m)))))



;; 队列的表示
(define (front-ptr queue)
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue)
  (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (set-front-ptr! queue (cdr (front-ptr queue)))))



;; 练习 3.21
;; queue存放的是两个指针，都是序列的指针。在Lisp标准输出函数中，如果传给它一个序列的指针，他将把所有的car 和 cdr对应的所有数据打印出来，所以打印出来的序列第一项是整个序列，第二项是序列的最后一项。由于delete-queue!过程仅仅修改queue的car指针，所以最后一项的数据依然可以被打印。

;; 下面的过程可以打印出队列里的数据。
(define (print-queue queue)
  (front-ptr queue))


;; 练习 3.22
(define (make-queue)
  (let ((front '())
        (rear '()))
    (define (empty-queue?)
      (null? front))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front new-pair)
               (set! rear new-pair))
              (else
               (set-cdr! rear new-pair)
               (set! rear new-pair)))))

    (define (delete-queue!)
      (if (empty-queue?)
          (error "DELETE! called with an empty queue")
          (set! front (cdr front))))

    (define (print-queue)
      front)

    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (error Undefined operation))))

    dispatch))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue! queue)
  ((queue 'delete-queue!)))

(define (print-queue queue)
  ((queue 'print-queue)))


(define q2 (make-queue))
(insert-queue! q2 'a)
(insert-queue! q2 'b)
(print-queue q2)

(delete-queue! q2)
(print-queue q2)

(delete-queue! q2)
(print-queue q2)



;; 练习 3.22

;; 一个具有
(define (new-deitem item prev next)
  (list item prev next))

(define (prev-deitem deitem)
  (cadr deitem))

(define (next-deitem deitem)
  (caddr deitem))

(define (item-deitem deitem)
  (car deitem))

(define (set-prev-deitem! deitem item)
  (set-car! (cdr deitem) item))

(define (set-next-deitem! deitem item)
  (set-car! (cddr deitem) item))

(define (make-deque)
  (let ((front '())
        (rear '()))
    (define (empty-queue?)
      (null? front))

    (define (front-deque)
      front)

    (define (rear-deque)
      rear)

    (define (front-insert-deque! item)
      (let ((deitem (new-deitem item '() '())))
        (cond ((empty-queue?)
               (set! front deitem)
               (set! rear deitem))
              (else
               (set-next-deitem! deitem front)
               (set! front deitem)))))

    (define (rear-insert-deque! item)
      (let ((deitem (new-deitem item '() '())))
        (cond ((empty-queue?)
               (set! front deitem)
               (set! rear deitem))
              (else
               (set-prev-deitem! deitem rear)
               (set! rear deitem)))))

    (define (front-delete-queue!)
      (if (empty-queue?)
          (error "DELETE! called with an empty queue")
          (begin
            (set-prev-deitem! (next-deitem front) '())
            (set! front (next-deitem front)))))

    (define (rear-delete-queue!)
      (if (empty-queue?)
          (error "DELETE! called with an empty queue")
          (begin
            (set-next-deitem! (prev-deitem rear) '())
            (set! rear (prev-deitem rear)))))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-deque) front-deque)
            ((eq? m 'rear-deque) rear-deque)
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'rear-insert-deque!) rear-insert-deque!)
            ((eq? m 'front-delete-queue!) front-delete-queue!)
            ((eq? m 'rear-delete-queue!) rear-delete-queue!)
            (else (error Undefined operation))))

    dispatch))

(define (empty-queue? queue)
  ((queue 'empty-queue?)))

(define (front-deque queue)
  (item-deitem ((queue 'front-deque))))

(define (rear-deque queue)
  (item-deitem ((queue 'rear-deque))))

(define (front-insert-deque! queue item)
  ((queue 'front-insert-deque!) item))

(define (rear-insert-deque! queue item)
  ((queue 'rear-insert-deque!) item))

(define (front-delete-queue! queue)
  ((queue 'front-delete-queue!)))

(define (rear-delete-queue! queue)
  ((queue 'rear-delete-queue!)))

;; test
(define q (make-deque))

(rear-insert-deque! q 'a)
(front-deque q)

(front-insert-deque! q 'b)
(front-deque q)

(front-delete-queue! q)
(front-deque q)


;; 3.3.3 表格的表示
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (car record)
        #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car record))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))


;; 二维表格的表示
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))


(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table))))))


(define (make-table)
  (let ((table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))


    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr table))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

(define operation-table (make-table))

(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'a 'b 1)
(get 'a 'b)




;; 练习 3.24
(define (make-table same-key?)
  (let ((table (list '*table*)))

    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr table))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

(define table2 (make-table
                (lambda (x1 x2)
                  (< (abs (- x1 x2)) 0.5))))

(define get (table2 'lookup-proc))
(define put (table2 'insert-proc!))

(put 100 90 'a)
(get 99.9 90)



;; 练习 3.25
;; 我们完全可以把多个关键码组成的表当做是一个关键码。
;; equals?过程就可以判断两个表是否为同一个表。同一性问题得到完好处理。


;; 练习 3.26
;; 这里只实现一维表格。假设按数值排序。
;; 这里的lookup跟之前习题几乎一模一样。换句话说，用树进行遍历的逻辑都是通用的。

(define (lookup key node)
  (cond ((empty-tree? node) #f)
        ((= (tkey node) key) (tvalue node))
        ((< (tkey node) key) (lookup key (right-branch node)))
        ((> (tkey node) key) (lookup key (left-branch node)))))


(define (insert! key value node)
  (cond ((empty-tree? node)
         (let ((newtree (make-tree key
                                   value
                                   (make-empty-tree)
                                   (make-empty-tree))))
           (set-car! node (car newtree))
           (set-cdr! node (cdr newtree))))
        ((= key (tkey node)) (set-tree-val node value))
        ((< key (tkey node)) (insert! key value (left-branch node)))
        ((> key (tkey node)) (insert! key value (right-branch node)))))

(define (make-table)
  (make-empty-tree))

(define (make-empty-tree)
  (cons '() '()))

(define (empty-tree? t)
  (equal? t (cons '() '())))


(define (make-tree k v left right)
  (list k v left right))

(define (tkey t)
  (car t))

(define (tvalue t)
  (cadr t))

(define (left-branch t)
  (caddr t))

(define (right-branch t)
  (cadddr t))

(define (set-tree-val t value)
  (set-car! (cdr t) value))


(define table3 (make-table))

(insert! 66 'aa table3)
(insert! 88 'bb table3)

(lookup 88 table3)



;; 3.3.4 数字电路的模拟器
;; 主要思路是先定义基础的概念，比如连线以及基本单元。并提供scheme的表示。
;; 数字电路的模拟器，也是可以应用于日常开发的各类系统。
;; 为什么不设计成入参是输入连线，返回是输出连线呢？如果这样设计，那么这个过程就是连线生成器了，也并不是一个组件了。所以，这个函数能代表什么，取决于它的返回值。
;; !! 各种基本功能块形成了这个语言的基本元素，将功能块连接起来就是这里的组合方法，而将特定的连接模式定义为过程就是这里的抽象方法。
;; 一个语言，需要基本元素，需要组合方法，需要抽象方法。

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))


(define (full-adder a b c-in sum c-out)
  (let ((s make-wire)
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1)
                        (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        (else 0)))


;; 练习 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1)
                        (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        (else 0)))


;; 练习 3.29
(define (or-gate a1 a2 output)
  (let ((n1 (make-wire))
        (n2 (make-wire))
        (a3 (make-wire)))
    (inverter a1 n1)
    (inverter a2 n2)
    (and-gate n1 n2 a3)
    (inverter a3 output)))

(define or-gate-delay (+ inverter-delay inverter-delay and-gate-delay))


;; 练习 3.30
(define (ripple-carry-adder ak bk c-in sk c-out)
  (if (and ak bk sk)
      (let ((c (if (> (length ak) 1)
                   (make-wire)
                   c-out)))
        (full-adder (car ak)
                    (car bk)
                    c-in
                    (car sk)
                    c)
        (ripple-carry-adder (cdr ak)
                            (cdr bk)
                            c
                            (cdr sk)
                            c-out))))

;; 一个全加器的时延等于两个半加器时延加上或门时延。而半加器的C时延是有一个与门决定的。所以一个全加器的C时延为以下：
(define full-adder-delay (+ and-gate-delay and-gate-delay or-gate-delay))

;; 级联进位加法器需要串联n个全加器，所以是以下：
(define (ripple-carry-adder-delay n)
  (* n full-adder-delay))



;; 线路的表示
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= new-value signal-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures)))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation"))))
    dispatch))


(define (call-each procs)
  (if (null? procs)
      'done
      (begin ((car procs))
             (call-each (cdr procse)))))


(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))


(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))


;; 练习 3.31
;; 当初始化一个组件时，传入的线路是有状态的，如果不立即调用，那么这些状态将停止传递。


;; 待处理表的实现
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))


(define (make-agenda (list 0)))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))


(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))


(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

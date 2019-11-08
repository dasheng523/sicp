;; 2.3.3

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set2) set1) (cons (car set2)
                                                 (intersection-set set1 (cdr set2))))
        (else (intersection-set set1 (cdr set2)))))


;; 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((uset (union-set set1 (cdr set2))))
                (if (element-of-set? (car set2) uset)
                    uset
                    (cons (car set2) uset))))))


(intersection-set (list 1 2 3) (list 2 2 0 4))
(union-set (list 1 2 3) (list 2 2 0 4))



;; 2.60
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (adjoin-set (car set2) (union-set set1 (cdr set2))))))

(union-set (list 1 2 3) (list 2 2 3))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set2) set1)
         (adjoin-set (car set2)
                     (intersection-set set1 (cdr set2))))
        (else (intersection-set set1 (cdr set2)))))

;; 下面的两个情况结果不一样，是对还是错呢？
(intersection-set (list 1 1 2 3 3) (list 1 2 2 3))
(intersection-set (list 1 2 2 3) (list 1 1 2 3 3))
;; 上面的结果应该是(list 1 1 1 2 2 2 3 3 3)才对。
;; TODO





;;;;;;;; 集合作为排序的表
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))


(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))))))



;; 2.61
(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) (adjoin-set x(union-set (list->tree (list 1 3 5 6 7))
           (list->tree (list 2 4 6 8 9))) (cdr set)))
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))


;; 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (union-set set1 (cdr set2)))
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2) (union-set set1 (cdr set2))))))

(union-set (list 2 4 6 8)
           (list 1 3 6 7))



;;;;;;; 集合作为二叉树
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (right-branch set)
                                      (adjoin-set x (right-branch set))))))


;; 2.63

;; 两个算法算出的结果是一样的。
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


(tree->list-1 (make-tree 7
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    (make-tree 5 '() '()))
                         (make-tree 9
                                    '()
                                    (make-tree 11 '() '()))))

(tree->list-2 (make-tree 7
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    (make-tree 5 '() '()))
                         (make-tree 9
                                    '()
                                    (make-tree 11 '() '()))))


(tree->list-1 (make-tree 3
                         (make-tree 1 '() '())
                         (make-tree 7
                                    (make-tree 5 '() '())
                                    (make-tree 9 '()
                                               (make-tree 11 '() '())))))

(tree->list-2 (make-tree 3
                         (make-tree 1 '() '())
                         (make-tree 7
                                    (make-tree 5 '() '())
                                    (make-tree 9 '()
                                               (make-tree 11 '() '())))))


(tree->list-1 (make-tree 5
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    '())
                         (make-tree 9
                                    (make-tree 7 '() '())
                                    (make-tree 11 '() '()))))

(tree->list-2 (make-tree 5
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    '())
                         (make-tree 9
                                    (make-tree 7 '() '())
                                    (make-tree 11 '() '()))))


;; 2.63 b

(make-tree 5
           (make-tree 3
                      (make-tree 1 '() '())
                      '())
           (make-tree 9
                      (make-tree 7 '() '())
                      (make-tree 11 '() '())))

;; 第一个算法 计算步骤
(append (tree->list-1 (left-branch tree))
        (cons (entry tree)
              (tree->list-1 (right-branch tree))))

(append (tree->list-1 (make-tree 3
                                 (make-tree 1 '() '())
                                 '()))
        (cons 5
              (tree->list-1 (make-tree 9
                                       (make-tree 7 '() '())
                                       (make-tree 11 '() '())))))

(append (tree->list-1 (make-tree 1 '() '()))
        (cons 3
              (tree->list-1 '())))

(append (tree->list-1 '())
        (cons 1
              (tree->list-1 '())))

;; 以此类推，第一个算法的计算步骤是O(3n)


;; 第二个算法 计算步骤
(make-tree 5
           (make-tree 3
                      (make-tree 1 '() '())
                      '())
           (make-tree 9
                      (make-tree 7 '() '())
                      (make-tree 11 '() '())))
(copy-to-list (left-branch tree)
              (cons (entry tree)
                    (copy-to-list (right-branch tree)
                                  result-list)))

(copy-to-list (make-tree 3
                         (make-tree 1 '() '())
                         '())
              (cons 5
                    (copy-to-list (make-tree 9
                                             (make-tree 7 '() '())
                                             (make-tree 11 '() '()))
                                  '())))

(copy-to-list (make-tree 7 '() '())
              (cons 9
                    (copy-to-list (make-tree 11 '() '())
                                  '())))


(copy-to-list '()
              (cons 11
                    (copy-to-list '()
                                  '())))

(copy-to-list (make-tree 3
                         (make-tree 1 '() '())
                         '())
              (list 7 9 11))

(copy-to-list (make-tree 1 '() '())
              (cons 3
                    (copy-to-list '()
                                  (list 7 9 11))))

;; 第二个算法的步数更少一点，O(2n)
;; 时间复杂度就是执行的代码数，如果算法是递归的，那么代码数差不多等于执行递归函数的数量。
;; 本题中的第二个算法在递归叶子节点中比第一个方案少了n个数量，所以时间上比较占优。



;; 2.64 a
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(list->tree (list 1 3 5 7 9 11))

;; 这个算法的思路就是先递归计算出左树，在用剩余的元素递归计算右树，最后整理返回。


;; 2.64 b
;; 时间复杂度是 O(n)
;; 因为递归计算左右子树，每次递归产生两次子递归，n的值减半，一直递归到n=0为止，总共产生的(partial-tree)数量是n。



;; 2.65

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((t1 (list->tree set1))
                    (t2 (list->tree set2)))
                (cond ((> (entry t1) (entry t2))
                       (append (union-set (tree->list-1 (left-branch t1))
                                          (tree->list-1 (make-tree (entry t2)
                                                                   (left-branch t2)
                                                                   '())))
                               (union-set (tree->list-1 (make-tree (entry t1)
                                                                   '()
                                                                   (right-branch t1)))
                                          (tree->list-1 (right-branch t2)))))
                      ((< (entry t1) (entry t2))
                       (append (union-set (tree->list-1 (left-branch t2))
                                          (tree->list-1 (make-tree (entry t1)
                                                                   (left-branch t1)
                                                                   '())))
                               (union-set (tree->list-1 (make-tree (entry t2)
                                                                   '()
                                                                   (right-branch t2)))
                                          (tree->list-1 (right-branch t1)))))
                      ((= (entry t1) (entry t2))
                       (tree->list-1
                        (make-tree (entry t1)
                                   (list->tree (union-set (left-branch t1)
                                                          (left-branch t2)))
                                   (list->tree (union-set (right-branch t1)
                                                          (right-branch t2)))))))))))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        (else (let ((t1 (list->tree set1))
                    (t2 (list->tree set2)))
                (cond ((> (entry t1) (entry t2))
                       (append (intersection-set (tree->list-1 (left-branch t1))
                                          (tree->list-1 (make-tree (entry t2)
                                                                   (left-branch t2)
                                                                   '())))
                               (intersection-set (tree->list-1 (make-tree (entry t1)
                                                                   '()
                                                                   (right-branch t1)))
                                          (tree->list-1 (right-branch t2)))))
                      ((< (entry t1) (entry t2))
                       (append (intersection-set (tree->list-1 (left-branch t2))
                                          (tree->list-1 (make-tree (entry t1)
                                                                   (left-branch t1)
                                                                   '())))
                               (intersection-set (tree->list-1 (make-tree (entry t2)
                                                                   '()
                                                                   (right-branch t2)))
                                          (tree->list-1 (right-branch t1)))))
                      ((= (entry t1) (entry t2))
                       (tree->list-1
                        (make-tree (entry t1)
                                   (list->tree (intersection-set (left-branch t1)
                                                          (left-branch t2)))
                                   (list->tree (intersection-set (right-branch t1)
                                                                 (right-branch t2)))))))))))



(union-set (list 1 3 5 6 7)
           (list 2 4 6 8 9))

(intersection-set (list 1 4 5 6 7)
                  (list 2 4 6 8 9))

;; 这种算法看起来就有点啰嗦，因为它进行很多次tree和list的转换，所以还可以进行优化。
;; 可以编写一个以两个树为参数的函数，而返回一颗合并后的树。这样就可以减少很多转换了。
;; 我就不写实现了。






;; 集合与信息检索
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))



;; 2.66
;; 时间复杂度是log(n)
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry set-of-records))) (entry set-of-records))
        ((> given-key (key (entry set-of-records))) (lookup given-key (right-branch set-of-records)))
        ((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))))


(define (key record)
  (car record))

(define (make-record key data)
  (cons key data))

;; test
(lookup 1 (make-tree (make-record 8 8)
                     (make-tree (make-record 4 4)
                                (make-tree (make-record 1 1) '() '())
                                (make-tree (make-record 5 5) '() '()))
                     (make-tree (make-record 10 10)
                                '()
                                (make-tree (make-record 11 11) '() '()))))



;; Huffman数的表示
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? 'leaf (car object)))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; 叶子也是树，但树不是叶子。而且因为树和叶子需要做一定区分，所以必须把叶子抽离出来。

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;; 2.67
(define (sample-tree)
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


(decode sample-message (sample-tree))



;; 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((or (null? tree) (not (has-symbol? symbol tree)))
         (error "not in tree"))
        ((leaf? tree) '())
        ((has-symbol? symbol (left-branch tree))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((has-symbol? symbol (right-branch tree))
         (cons 1 (encode-symbol symbol (right-branch tree))))))

(define (has-symbol? symbol tree)
  (define (in-list? ls)
    (cond ((null? ls) #f)
          ((eq? symbol (car ls)) #t)
          (else (in-list? (cdr ls)))))
  (let ((ss (symbols tree)))
    (in-list? ss)))


(encode (decode sample-message (sample-tree))
        (sample-tree))



;; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (define (tree-iter ls)
    (cond ((= 1 (length ls)) ls)
          (else (tree-iter (adjoin-set
                            (make-code-tree (car ls) (cadr ls))
                            (cddr ls))))))
  (car (tree-iter pairs)))

(generate-huffman-tree (list '(B 2) '(A 4) '(C 1) '(D 1)))



;; 2.70
(define song-code-tree
  (generate-huffman-tree (list '(a 2)
                               '(na 16)
                               '(boom 1)
                               '(sha 3)
                               '(get 2)
                               '(yip 9)
                               '(job 2)
                               '(wah 1))))


(define song-message '(get a job
                           sha na na na na na na na na
                           get a job
                           sha na na na na na na na na
                           wah yip yip yip yip yip yip yip yip yip
                           sha boom))

;; 84个二进制位
(length (encode song-message song-code-tree))

;; 如果使用定长编码，每个词需要3位二进制，一共32个词，所以是 32 * 3 = 96 位



;; 2.71
;; n=5, n=10的形状差不多，是一棵右分支只有一个叶子节点的树。
;; 从根节点开始，第一个右节点是频率最高的符号，用一个二进制位即可表示。
;; 最左边的叶子节点是频率最低的符号，它的需要用n-1个二进制位表示。


;; 2.72
;; 如果是一个平衡二叉树，那增长速率是O(n)。
;; 因为每次调用都需要检查符号表，第一次检查需要n步，第二次检查需要n/2步，以此循环，以此最后一次需要1步。所以最坏的步数加起来就是2n步。
;; 如果不是，以2.71为例的树，第一次检查需要n步，第二次检查需要n-1步，以此循环n-1次，最坏的情况就是n的平方。

;; 最频繁的符号位于第一个右分支里，所以只需要检查一次符号表即可，所以增长速率是n
;; 最不频繁的符号位于最左边的分支里，需要检查n的平方次，上面说过了。



;; 复数的表示
;; 数据抽象保证了下面的函数都能正常运行
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


;; 选择函数和构造函数有两种实现方式
;; 方式一，使用直角坐标系
(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y)
  (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;; 方式二，使用极坐标形式
(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z)
  (car z))

(define (angle z)
  (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a)
  (cons r a))



;; 带标志数据
;; 最小允诺原则，就是确保系统能正常运行的最低标准，可以包容多个不同实现，并保持表示方式的不确定。
;; 借助类型标志，可以选择不同的选择函数处理不同的数据。

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))


;; 使用标志修改后的表示
;; 方式一，使用直角坐标系
(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle-rectangular z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

;; 方式二，使用极坐标形式
(define (real-part-polar z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part-polar z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude-polar z)
  (car z))

(define (angle-polar z)
  (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
              (cons r a)))

;; 注意到这里的选择函数并没有以标识数据为参数，而是保持以前的实现。
;; 这也是“最小允诺原则”，修改最少代码实现功能，更何况还要写下面的适配代码。

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else
         (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else
         (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else
         (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else
         (error "Unknown type -- ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; 在将数据对象从一个层次传到另一个层次的过程中，这种剥去和加上标志的规范方式能成为一种重要的组织策略。


;; 这个例子中的复数，为什么不采用约定界面的方式呢？采用约定界面方式如何构造复数呢？
;; 这个问题是伪命题，复数加减乘除之后，还是一个复数。就像是画家一样，进行转换之后，还是画家。
;; 约定界面，就是复数。



;; 数据导向的程序设计和可加性
;; 上门的方案不具有可加性，因为通用型界面过程需要知道所有的实现，而且名字容易冲突。
;; 数据导向程序设计，是将程序抽象成一组数据，并基于数据而设计各种操作。

(define (install-rectangular-package)
  (define (real-part z) (car z))

  (define (imag-part z) (cdr z))

  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))

  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-real-imag x y)
    (cons x y))

  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (install-polar-package)
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))

  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (magnitude z)
    (car z))

  (define (angle z)
    (cdr z))

  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  (define (make-from-mag-ang r a)
    (cons r a))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar r a)))


(add-complex (make-from-real-imag 1 2)
             (make-from-real-imag 2 3))



;; 2.73 a
;; 上面的求导过程主要是从数据表中找到对应的求导，对应类型的过程，然后进行调用。
;; 无法将number? 和 same-variable? 是因为数字和变量不能car和cdr，否则会报错的。


;; 2.73 b
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (install-deriv-sum)
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (put 'deriv '+ deriv-sum)

  'done)

(define (install-deriv-product)
  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  (put 'deriv '* deriv-product)
  'done)


;; 2.73 c
(define (install-deriv-exponentiation)
  (define (deriv-exponentiation exp var)
    (make-product (make-product (exponent exp)
                                (make-exponentiation (base exp)
                                                     (- (exponent exp) 1)))
                  (deriv (base exp) var)))

  (put 'deriv '** deriv-exponentiation)
  'done)


;; 2.73 d
;; 需要修改每个安装程序里面的put的前两个参数的顺序。而且需要重新执行安装过程。



;; 2.74 a
;; 一个人事文件的数据抽象应该是:company-name, file-data, make-company-file
;; 文件对应的公司名称
;; tip 如果需要判断多种表示和对应的通用界面是否能写出来，关键需要看标志数据是否能在通用界面过程拿得到。如果拿不到，那就肯定写不出来。 company-name需要写多个依赖分支的表示，那是写不出来的。
(define (company-name file))

;; 文件数据
(define (file-data file))

;; 获取文件的第一条记录
(define (first-record file))

;; 除第一条外的其他记录
(define (rest-record file))

;; 是否是空文件
(define (empty-file? file))

;; 构建函数,参数是集合数据
(define (get-company-A-file))

(define (get-company-B-file))


;; 不同的公司有不同的实现方式
(define (install-file-A)
  (define (get-record-A k data))
  (define (first-record file))
  (define (rest-record file))
  (define (empty-file? file))

  (put 'get-record 'A get-record-A)
  (put 'first-record' A first-record)
  (put 'rest-record' A rest-record)
  (put 'empty-file?' A empty-file?))


(define (install-file-B)
  (define (get-record-B k data))
  (define (first-record file))
  (define (rest-record file))
  (define (empty-file? file))

  (put 'get-record 'B get-record-B)
  (put 'first-record 'B first-record)
  (put 'rest-record' B rest-record)
  (put 'empty-file?' B empty-file?))

;; 通用界面
(define (get-record k file)
  ((get 'get-record (company-name file))
   k
   (file-data file)))

(define (first-record file)
  ((get 'first-record (company-name file))
   file))

(define (rest-record file)
  ((get 'rest-record (company-name file))
   file))

;; 2.74 b
;; 先设想一个雇员记录的数据抽象应该是:
;; 雇员名字
(define (name record))

;; 雇员收入
(define (salary record))

;; 雇员地址
(define (address record))

;; 不同的公司，雇员记录结构也是不一样的，所以需要封装不同公司的操作。
(define (install-record-A)
  (define (name record))
  (define (salary record))
  (define (address record))

  (put 'name 'A name)
  (put 'salary 'A salary)
  (put 'address 'A address))

(define (install-record-B)
  (define (name record))
  (define (salary record))
  (define (address record))

  (put 'name 'B name)
  (put 'salary 'B salary)
  (put 'address 'B address))

;; 界面过程
(define (get-salary k file)
  (let ((record (get-record k file))
        (cname (company-name file)))
    ((get 'salary cname) record)))



;; c
;; 从文件中找到对应名字的记录
(define (find-record-by-name name file)
  (cond ((empty-file? file) #f)
        ((eq? name ((get 'name (company-name file))
                    (first-record file)))
         (first-record file))
        (else (find-record-by-name name (rest-record file)))))

(define (find-employee-record name files)
  (cond ((null? files) #f)
        (else (or (find-record-by-name name (car file))
                  (find-employee-record name (cdr files))))))


;; d
;; 需要编写 install-file, install-record，以及构造新公司的file的过程，然后安装它们即可。



;; 消息传递风格
;; 消息传递风格也是一种强大的技术，它将数据对象设想为一个实体，表示为一个函数，并通过消息的方式接受所需操作的名字。

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op args)
  ((args op)))

;; 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)


;; 2.76
;; 显式分派风格
;; 这种风格中，如果加入新的数据类型，就需要编写新数据类型的操作，还需要修改通用型函数里面的内容。
;; 如果加入新的操作，则需要在每个数据类型中加入新操作，并加入新的通用型操作。如果新操作名冲突，那就涉及到修改操作了。

;; 数据导向风格
;; 此风格中，如果加入新数据类型，只需要编写新数据类型对应的操作，封装到安装包中，并执行安装程序。
;; 如果加入新的操作，就需要在每个安装包中新增对应的操作，并加入到操作类型表格，最后重新执行所有安装程序。

;; 消息传递风格
;; 此风格中，如果加入新数据类型，那就编写新的构造函数，其他内容不需要修改。
;; 如果加入新操作，就修改每个数据对象的过程，并加入新的操作内容。

;; 如果频繁新增数据类型，显示分派风格肯定是最不好用的。数据导向风格比消息传递风格多了执行安装程序这一步，所以是消息传递最好用。
;; 如果频繁新增操作，消息传递风格需要修改原有构造函数，数据导向风格需要修改安装函数，而显示分派风格只有新操作名冲突才需要修改原有函数。这一点或许不是很重要。
;; 频繁新增操作时，数据导向风格必须重新执行所有安装程序，这一点比不上消息传递，所以还是消息传递更好一些。

;; 此外，在消息传递风格中，如果操作需要接受多个参数，会使设计变得更复杂一些。不过这也是一个仁者见仁智者见智的问题。

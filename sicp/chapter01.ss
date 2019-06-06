;;;;;;;;;;
;;;;;;;;;; 1.1 课程代码

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (sqrt-iter guess x)
  (display guess)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 4)

;;;;;;;;;; exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 1 2) 3 5)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;; 执行的时候没有收敛，程序执行无法停止。
;; 函数形式需要等待所有子节点处理完成才能返回结果。即使good-enount是真，程序还是会继续执行sqrt-iter函数，导致了无穷尽执行。
;; 由此可见函数的局限性，当递归的时候一定要确保函数的参数能够顺利求值。


;;;;;;;;;; exercise 1.7
;; 一旦开方数字比精度还小，就难以确保正确性了，只能不停调整精度才可以。小数平方之后得到的结果更小。
;; 对于非常大的数，比如14位数的时候，就不能运行了，一直卡着。到底为什么呢？
;; 我打印了中间结果，发现死循环了，也就是说good-enough一直为假。可能是由于计算过程中精度不停增加达到了语言级别的限制，所以在小数部分将会逐步失真了。当失真进度超过了good-enough的条件，就会失效了。比如编程语言只能计算20位数的精度，其中包括整数部分和小数部分，如果整数部分占了18为，那么小数部分就只能占2位数了。这样对于大数而言就没办法处理小数的判断了。
(sqrt 0.0000000345)  ;0.01889
;(sqrt 34504439321901) ;将失真，导致死循环

(define (good-enough? old-guess new-guess x)
  (< (/ (abs (- old-guess new-guess)) x) 0.001))

(define (sqrt-iter guess1 guess2 x)
  (if (good-enough? guess1 guess2 x)
      guess2
      (sqrt-iter guess2
                 (improve guess2 x)
                 x)))

(define (sqrt x)
  (sqrt-iter x 1.0 x))

(square (sqrt 0.000000000003453291))
(sqrt 345044393219022491)
;; 这种算法可以计算非常大或者非常小的值，因为good-enough的条件是根据比值来定的。对于小的数字它可以比值可以保证good-enount的精度，对于非常大的数字，小数部分会因为比值而保持精度。




;;;;;;;;;; exercise 1.8
;;;;;;;;;;;;;;;;;;;;;
(define (lifan-good-enough? guess1 guess2 x)
  (good-enough? guess1 guess2 x))

(define (lifang-improve guess x)
  ((- n 3)/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (lifang-iter guess1 guess2 x)
  (if (lifan-good-enough? guess1 guess2 x)
      guess2
      (lifang-iter guess2
                   (lifang-improve guess2 x)
                   x)))

(define (lifanggen x)
  (lifang-iter x 1.0 x))

(lifanggen 27)

;;;;;;;;;;;;;;;;;;;;;;


;; 课本代码
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


;;;;;;;;;;
;;;;;;;;;; 课程 1.2
;; 1.2.1
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


;;;;;;;;;; exercise 1.9
;; 第一种
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc ((* a (square b))+ 0 5)))))

;; (my-fast-expt-iter )第二种
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
;; 第一种是递归计算过程，第二种是迭代计算过程


;;;;;;;;;; exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
...
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;;;;;
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 1 (A 2 2))))
(A 1 (A 1 (A 1 (A 1 (A 2 1)))))
(A 1 (A 1 (A 1 (A 1 2))))
;;;;;
(A 3 3)

(define (f n) (A 0 n)) ;; 2n
(define (g n) (A 1 n)) ;; 2^n
(define (h n) (A 2 n)) ;; 2^(2^n)


;;;;;;;;;;
;;;;;;;;;; 1.22
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
;; TODO 为什么这个的Fib(n)空间需求正比于树的最大深度？
;; TODO 为什么这个的Fib的叶子节点数量等于Fib(n+1)？


(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b)
                  a
                  (- count 1))))
  (fib-iter 0 1 n))


;; 换零钱方式统计
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))




;;;;;;;;;; exercise 1.11
;; 树形递归计算过程
(define (fun11-1 n)
  (if (< n 3)
      n
      (+ (fun11-1 (- n 1))
         (* 2 (fun11-1 (- n 2)))
         (* 3 (fun11-1 (- n 3))))))

;; 迭代计算过程
;; a = a + 2b + 3c
;; b = a
;; c = b
;; 直到 n < 3, 取 a 值
(define (fun11-2 n)
  (if (< n 3)
      n
      (fun11-2-iter (fun11-2 2)
                    (fun11-2 1)
                    (fun11-2 0)
                    n)))

(define (fun11-2-iter a b c n)
  (if (< n 3)
      a
      (fun11-2-iter (+ a
                       (* 2 b)
                       (* 3 c))
                    a
                    b
                    (- n 1))))


;;;;;;;;;; exercise 1.12
(define (get-triangle-val n i)
  (cond ((or (< n 1) (< i 1) (> i n)) 0)  ;异常情况
        ((or (= n 1) (= i 1) (= i n)) 1)
        (else (+ (get-triangle-val (- n 1) (- i 1))
                 (get-triangle-val (- n 1) i)))))


;;;;;;;;;; exercise 1.13
(define (exp num n)
  (if (= n 0)
      1
      (* num
         (exp num
                 (- n 1)))))

(define golden-val (/ (+ 1 (sqrt 5)) 2))

(define (near-num n)
  (/ (exp golden-val n) (sqrt 5)))

;; 如果以下成立
(= (fib n) (near-num n))
;; 那么只要证明，以下成立即可
(= (fib (+ n 1))
   (near-num (+ n 1)))

;; 进行代换
;; 1
(= (fib (+ n 1))
   (+ (fib n) (fib (- n 1))))

;; 2
(= (near-num (+ n 1))
   (* (near-num n) golden-val)
   (* (fib n) golden-val))

;; 黄金分割点的数学定义是： golden-val * golden-val = golden-val + 1
(= )
;; 稍微转换以下得到以下等式
(= golden-val
   (/ (+ (fib n) (fib (- n 1)))
      (fib n)))
;; 将上面的等式代换 等式2 之后
(= (* (fib n) golden-val)
   (* (fib n)
      (/ (+ (fib n) (fib (- n 1)))
         (fib n)))
   (+ (fib n) (fib (- n 1)))
   (fib (+ n 1)))
;; 所以
(= (near-num (+ n 1) (fib (+ n 1))))
;; 额，我只写关键部分证明，归纳法的起始就不写了。


;;;;;;;;;; exercise 1.14
;; 图已经在本子上画了，这里不画了。
;; 图形跟fib的差不多。
;; 空间增长阶是O(n)，
;; 步数增长阶是O()，我现在只知道是指数级别的，但是基数到底是什么呢？
;; 我知道叶子节点数是(count-change n)，所以增长阶是O((count-change n))
;; 这道题存疑 TODO


;;;;;;;;;; exercise 1.15
(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05))))))
;; 所以总共执行5次
;; 步数是a不停除以3，直到商小于0.1的次数。即：
(define (step-count n)
  (define (step-iter a i)
    (if (< a 0.1)
        i
        (step-iter (/ a 3) (+ i 1))))
  (step-iter n 0))
(step-count 12.15)
;; 所以步数增长阶是log3 (n)
;; 空间增长阶也是O(log3 (n))
;; TODO 还是不太能理解什么是空间增长阶，指的是要存储的数量对比。




;;;;;;;;;; 1.2.4 求幂
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))


(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt n (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))



;;;;;;;;;; exercise 1.16
(define (my-fast-expt b n)
  (define (my-fast-expt-iter b n rs)
    (cond ((= n 0) rs)
          ((= n 1) (* b rs))
          ((even? n) (my-fast-expt-iter (square b) (/ n 2) rs))
          (else (my-fast-expt-iter b (- n 1) (* b rs)))))
  (my-fast-expt-iter b n 1))



;;;;;;;;;; exercise 1.17
(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))

(define (multi a b)
  (cond ((= b 0) 0)
        ((= a 0) 0)
        ((= b 1) a)
        ((even? b) (double (multi a (halve b))))
        (else (+ (multi a (- b 1)) a))))

(multi 60 10)


;;;;;;;;;; exercise 1.18
(define (multi-fast a b)
  (define (multi-iter a b rs)
    (cond ((= b 0) rs)
          ((= b 1) (+ a rs))
          ((even? b) (multi-iter (double a) (halve b) rs))
          (else (multi-iter a (- b 1) (+ rs a)))))
  (multi-iter a b 0))

(multi-fast 26 180)


;;;;;;;;;; exercise 1.19
;; 令 a0，b0为初始化值。
;; 应用一次之后 a1 = b0*q + a0*p + a0*q, b1 = b0*p + a0*q;
;; 应用两次之后 a2 = b1*q + a1*p + a1*q, b2 = b1*p + a1*q；
;; 经过代入转换之后 a2 = (2*p*q+q*q) * b0 + (p*p + q*q) * a0 + (2*p*q+q*q) * a0;
;; 所以 q' = (2*p*q+q*q), p' = (p*p+q*q);
;; 用数学归纳法可以简单证明，这里就不证明了。
(define (fib2 n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p)
                        (* q q))
                     (+ (* 2 p q)
                        (* q q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))




;;;;;;;;;; exercise 1.20
(remainder 206 40) 6
(remainder 40 6) 4
(remainder 6 4) 2
(remainder 4 2) 0
(remainder 2 0)

;; 应用序只需要执行5次即可。
;; 正则序很繁琐，每次if都要重复嵌套执行多次。
;; 16次
;; 正则序遇到if首先求值，否则会变成死循环的。
;; TODO 先留存吧，后面慢慢验证。




;;;;;;;;;; 1.26实例 素数检测
(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


;;;;;;;;;; exercise 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)



;;;;;;;;;; exercise 1.22
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))


(define (runtime)
  (define thetime (current-time))
  (+ (time-second thetime)
     (/ (time-nanosecond thetime) 1000000000.0)))


(define (search-for-primes start-num end-num)
  (cond ((even? start-num)
         (search-for-primes (+ 1 start-num) end-num))
        (else
         (cond ((< start-num end-num)
                (timed-prime-test start-num)
                (search-for-primes (+ start-num 2) end-num))))))


(search-for-primes 100000 100100)
(search-for-primes 1000000 1000100)
(search-for-primes 10000000 10000100)
(search-for-primes 100000000 100000100)
(search-for-primes 1000000000 1000000100)
(search-for-primes 10000000000 10000000100)
(search-for-primes 100000000000 100000000100)


(define (get-exetime n)
  (define start-time (runtime))
  (prime? n)
  (- (runtime) start-time))

(define (diff-time num1 num2)
  (/ (get-exetime num2) (get-exetime num1)))

(define (print-times num1 num2 times)
  (cond ((> times 0)
         (newline)
         (display (diff-time num1 num2))
         (newline)
         (print-times num1 num2 (- times 1)))))

(sqrt 10)

(print-times 100003 1000003 10)
(print-times 10000000019 100000000003 10)

(diff-time 10000000019 100000000003)
(diff-time 100003 1000003)


;; 测试结果，跟(sqrt 10)倍相差很近有时相差很远。
;; 即使判断同样的两个数，消耗的时间也不一样。
;; 进一步测试之后，数值越大，相差倍数越来越相近(sqrt 10)，可以看见其中有一些固定步数消耗时间，当其占比变少了之后，就会越来越准确。
;; 除去多余的display,newline这些固定步数，计算步数最多的是prime?函数，这个函数增长阶是O(sqrt n)，而实际情况要看这个增长阶占总时长的比重来定，如果比重较小，是看不出增长阶的变化率的，如果比重较大，就会遵守这个增长阶。



;;;;;;;;;; exercise 1.23

(define (prime1? n)
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (prime2? n)
  (define (next num)
    (cond ((= num 2) 3)
          (else (+ num 2))))
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)))))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (get-exetime1 n)
  (define start-time (runtime))
  (prime1? n)
  (- (runtime) start-time))

(define (get-exetime2 n)
  (define start-time (runtime))
  (prime2? n)
  (- (runtime) start-time))




(search-for-primes 10000000000 10000000100)

(/ (get-exetime1 10000000000037) (get-exetime2 10000000000037))
(/ (get-exetime1 100000000003) (get-exetime2 100000000003))
(/ (get-exetime1 10000000019) (get-exetime2 10000000019))


;; 执行了之后发现prime2的版本速度与prime1的比值在2左右。可能是我没有加上IO操作吧，所以比较准确一些。
;; 早上测试的时候总是小于2，而下午测试总是大于2。
;; TODO 



;;;;;;;;;; exercise 1.24
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (get-exetime3 n)
  (define start-time (runtime))
  (fast-prime? n 10)
  (- (runtime) start-time))


(/ (get-exetime1 10000000000037) (get-exetime3 10000000000037))
(/ (get-exetime1 100000000003) (get-exetime3 100000000003))
(/ (get-exetime1 10000000019) (get-exetime3 10000000019))



;;;;;;;;;; exercise 1.25
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt n (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod2 base exp m)
  (remainder (fast-expt base exp) m))


(define (fermat-test2 n)
  (define (try-it a)
    (= (expmod2 a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime2? n times)
  (cond ((= times 0) #t)
        ((fermat-test2 n) (fast-prime2? n (- times 1)))
        (else #f)))

(define (get-exetime4 n)
  (define start-time (runtime))
  (fast-prime2? n 10)
  (- (runtime) start-time))



(remainder (square (expmod 3 2 4)) 4)
(remainder (square (remainder (square (expmod 3 1 4)) 4)) 4)
(remainder (square (remainder (square (remainder (* 3 (expmod 3 0 4)) 4)) 4)) 4)
(remainder (square (remainder (square (remainder (* 3 1) 4)) 4)) 4)
(remainder (square (remainder (square 3) 4)) 4)
(remainder (square 1) 4)

;; (get-exetime4 10000000019)
;; 压根跑不起来。
;; 这是由于课本中的expmod算法在回归的时候进行模运算，使得中间产生的数值控制在一定范围。
;; 而使用fast-expt的算法，会不断计算非常庞大的数值，最终导致了对于大数字难以计算其指数。
;; 设计算法的时候应该尽量避免使用指数运算，哪怕是简单的数值。



;;;;;;;;;; exercise 1.26
;; 在应用序计算模型中，会先将参数先求值，而在这个实例中，(expmod ...)每次递归都被调用两次，以此循环，形成了树形结构，高度是log(n)，叶子节点是n，所以增长阶是N。



;;;;;;;;;; exercise 1.27
(define (check-carmichael num)
  (define (check-carmichael-iter a)
    (cond ((= a num) #t)
          ((= (expmod a num num) a) (check-carmichael-iter (+ a 1)))
          (else #f)))
  (check-carmichael-iter 1))


(check-carmichael 561)
(check-carmichael 1105)
(check-carmichael 1729)
(check-carmichael 2465)
(check-carmichael 2821)
(check-carmichael 6601)



;;;;;;;;;; exercise 1.28
(define (check-out-square a n)
  (define result (remainder (square a) n))
  (if (and (not (= a 1))
           (not (= a (- n 1)))
           (= result 1))
      (and (display result) 0)
      result))


(define (expmod-miller base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-out-square (expmod-miller base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod-miller base (- exp 1) m))
                    m))))

;; 这是题目要求的答案
(define (fermat-test-miller n)
  (define (try-it)
    (= (expmod-miller (+ (random (- n 2)) 1)
                      (- n 1)
                      n)
       1))
  (define (retry-times times)
    (cond ((= times 100) #t)
          ((try-it) (retry-times (+ times 1)))
          (else #f)))
  (retry-times 0))


;; 用来验证算法正确性
(define (check-test-miller n)
  (define (try-it a)
    (= (expmod-miller a (- n 1) n) 1))
  (define (retry-num num)
    (cond ((= num (- n 1)) #t)
          ((try-it num) (retry-num (+ num 1)))
          (else (display num) #f)))
  (retry-num 1))

(check-test-miller 7)


;; 素数检查
(fermat-test-miller 7)
(fermat-test-miller 17)
(fermat-test-miller 100003)
(fermat-test-miller 10000000000037)


;; carmichael检查
(fermat-test-miller 561)
(fermat-test-miller 1105)
(fermat-test-miller 1729)
(fermat-test-miller 2465)
(fermat-test-miller 2821)
(fermat-test-miller 6601)



;;;;;;;;;; 1.3
;;;;;;;;;;
(define (cube n)
  (* n n n))

(define (sum-intergers a b)
  (if (> a b)
      0
      (+ a (sum-intergers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))


(define (pi-num a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-num (+ a 4) b))))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
;; (sum-cubes 1 10)

(define (identity x) x)
(define (sum-intergers a b)
  (sum identity a inc b))
;; (sum-intergers 1 4)


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
;; (* 8 (pi-num 1 1000))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)
;; (integral cube 0 1 0.001)




;;;;;;;;;; 1.29
(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (y-fn k) (f (+ a (* k h))))
  (define (simpson-term k)
    (cond ((or (= k 0) (= k n)) (y-fn k))
          ((even? k) (* 2 (y-fn k)))
          (else (* 4 (y-fn k)))))
  (* (/ h 3.0)
     (sum simpson-term 0 inc n)))

(integral-simpson cube 0 1 1000)



;;;;;;;;;; 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


(sum identity 1 inc 4)




;;;;;;;;;; 1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial a b)
  (product identity a inc b))



(define (product-pi)
  (define (pi-term i)
    (if (even? i)
        (/ (+ 2.0 i) (+ 1 i))
        (/ (+ 1 i) (+ 2 i))))
  (* (product pi-term 1 inc 1000000)
     4))

(product-pi)

(define (product term a next b)
  (cond ((> a b) 1)
        (else (* (term a) (product term (next a) next b)))))



;;;;;;;;;; 1.32

;; 递归式
(define (accumulate combiner null-value term a next b)
  (cond ((> a b) null-value)
        (else (combiner (term a)
                        (accumulate combiner null-value term (next a) next b)))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(product identity 1 inc 4)


(define (sum term a next b)
  (accumulate + 0 term a next b))

(sum identity 1 inc 4)


;; 迭代式
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))


;;;;;;;;;; 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a)
                  (combiner result (term a)))
            (iter (next a)
                  result))))
  (iter a null-value))


(define (sum-prime a b)
  (filtered-accumulate prime?
                       +
                       0
                       identity
                       a
                       inc
                       b))


(sum-prime 1 11) ;;29

(define (min a b)
  (if (> a b)
      b
      a))

(define (gcd a b)
  (define min-num (min a b))
  (define (gcd-iter a b n)
    (cond ((= n 1) 1)
          ((and (divides? n a) (divides? n b)) n)
          (else (gcd-iter a b (- n 1)))))
  (gcd-iter a b min-num))


(define (product-prime n)
  (define (each-prime? a)
    (= (gcd a n) 1))
  (filtered-accumulate each-prime? * 1 identity 1 inc n))


(product-prime 10)   ;189
(* 1 3 7 9)



;;;;;;;;;; 1.3.2 用lambda构造过程
(lambda (x) (+ x 4))
(lambda (x) (/ 1.0 (* x (+ x 2))))


(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))





;;;;;;;;;; 1.34
(define (f g)
  (g 2))

(f square)
(f (lambda (z) (* z (+ z 1))))

(f f)
(f (lambda (g) (g 2)))
((lambda (g) (g 2)) 2)
(2 2)
;; 本质上就是执行了这个过程，所以报错了。




;;;;;;;;;; 1.3.3 过程作为一般性的方法
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? a b)
  (< (abs (- a b)) 0.001))


(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of oppsite sign")))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)




(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)




;;;;;;;;;; 1.35

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;; 黄金分割的定义 $是黄金分割点
$^2 = $+1
两边除以$，得
$ = 1 + 1/$
所以问题得证了。




;;;;;;;;;; 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)

(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2)

;; 使用平均阻尼法，步骤明显少了很多。大概是1/3个步数。





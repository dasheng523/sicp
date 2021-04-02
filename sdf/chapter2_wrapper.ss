;;;; 2.3 Wrappers
;; 临时心得
;; 当需要扩展的时候，我一般在某个函数上多加一个参数，然后修改其内部实现。是的这样看来会非常快。但这会隐藏程序背后的逻辑。
;; 应该换成什么样的思维呢？我想要的目的是把背后的逻辑抽离出来。但一般来说背后的逻辑不会非常清晰。应怎么做呢？
;; 背后的逻辑就是不变的逻辑。就拿这个程序，不管单位如何变化，不变的是gas-law-volume的逻辑。
;; 找到背后的逻辑，也就是找到不变的逻辑。
;; 找到之后，就封装变化的逻辑了。变化的逻辑就可借助于领域语言。

;; 关注点分离就是将某段逻辑分成不同的部分。一般来说就是把主逻辑和从逻辑分开来。

(load "../g-point/startup.ss")
(import (common combinator))

(define (gas-law-volume pressure temperature amount)
  (/ (* amount gas-constant temperature) pressure))

(define gas-constant 8.3144621)

(define (sphere-radius volume)
  (expt (/ volume (* 4/3 pi)) 1/3))

(define pi (* 4 (atan 1 1)))

(define (make-unit-conversion f1 f2)
  (case-lambda
    [() (cons f1 f2)]
    [args (apply f1 args)]))

(define (unit:* u1 u2)
  (make-unit-conversion
   (compose u2 u1)
   (compose (unit:invert u1) (unit:invert u2))))

(define (units:* . units)
  (let ([n (length units)])
    (cond
     [(= n 1) (car units)]
     [(= n 2) (unit:* (car units) (cadr units))]
     [(> n 2) (apply units:* (unit:* (car units) (cadr units))
                     (cddr units))]
     [else (error "入参数量错误")])))


(define (unit:invert u)
  (let ([fs (u)])
    (make-unit-conversion
     (cdr fs)
     (car fs))))

(define (unit:/ u1 u2)
  (unit:* u1 (unit:invert u2)))

(define (unit:expt u n)
  (assert (exact-nonnegative-integer? n))
  (if (= n 1)
      u
      (unit:* u (unit:expt u (- n 1)))))




;; (sphere-radius (conventional-gas-law-volume 14.7 68 1))


(define (unit-specializer procedure implicit-output-unit . implicit-input-units)
  (define (specializer specific-output-unit . specific-input-units)
    (let ((output-converter (make-converter implicit-output-unit specific-output-unit))
          (input-converters (map make-converter specific-input-units implicit-input-units)))
      (define (specialized-procedure . arguments)
        (output-converter
         (apply procedure
                (map (lambda (converter argument) (converter argument))
                     input-converters arguments))))
      specialized-procedure))
  specializer)


;; 创建一个单位节点
(define (create-unit-node u)
  (list u))

;; 在一个节点上挂载一个叶子
(define (unit-node-mount node leaf conversion)
  (append node (list (cons leaf conversion))))

;; 获取节点的(叶子和转换器)的pair组成的List
(define (unit-node-leafs node)
  (cdr node))

;; 获取pair的单位
(define (leaf-unit p)
  (car p))

;; 获取pair的转换器
(define (leaf-conver p)
  (cdr p))

;; 节点table
(define unit-node-table (make-eqv-hashtable))

;; 获取单位节点
(define (get-unit-node u)
  (hashtable-ref unit-node-table u #f))


(define (register-unit-conversion u1 u2 conversion)
  (define (register u1 u2 conversion)
    (let ([node (or (get-unit-node u1)
                    (create-unit-node u1))])
      (hashtable-set! unit-node-table
                      u1
                      (unit-node-mount node u2 conversion))))
  (register u1 u2 conversion)
  (register u2 u1 (unit:invert conversion)))



;; (make-converter 'fahrenheit 'celsius)
(define (make-converter u1 u2)
  (cond
   [(and (simple-unit? u1) (simple-unit? u2))
    (let ([conversions (find-unit-path-conversion u1 u2)])
      (if conversions
          (apply units:* conversions)
          (error 'make-converter (list "找不到单位换算路径" u1 u2))))]
   [(and (compound-unit? u1) (compound-unit? u2))
    (let ([op1 (compound-unit-operator u1)]
          [op2 (compound-unit-operator u2)])
      (assert (eq? op1 op2))
      (let ([composer (find-operator-composer op)])
        (assert composer)
        (composer (make-converter (compound-unit-fst u1) (compound-unit-fst u2))
                  (make-converter (compound-unit-sec u1) (compound-unit-sec u2)))))]))





;; 判断是否是简单单位
;; (simple-unit? 'fahrenheit)
(define (simple-unit? u)
  (symbol? u))

(define (simple-unit-eq? u1 u2)
  (eq? u1 u2))

;; 寻找两个简单单位之间的转换器
;; ((car (find-unit-path-conversion 'fahrenheit 'celsius)) 66)
;; ((car (find-unit-path-conversion 'celsius 'kelvin)) 66) TODO 这里死循环了
(define (find-unit-path-conversion u1 u2)
  (let loop ([plist (unit-node-leafs (get-unit-node u1))])
    (if (null? plist)
        #f
        (let ([item (car plist)])
          (if (simple-unit-eq? u2 (leaf-unit item))
              (list (leaf-conver item))
              (let ([conver-path (find-unit-path-conversion (leaf-unit item) u2)])
                (if conver-path
                    (cons (leaf-conver item) conver-path)
                    (loop (cdr plist)))))))))

;; 创建一个复合单位
(define (make-compound-unit op u1 u2)
  (list op u1 u2))

;; 判断是否是复合单位
(define (compound-unit? u)
  (and (pair? u) (symbol? (car u))))

;; 获取复合单位的操作符
(define (compound-unit-operator u)
  (car u))

;; 获取复合单位的第一个单位
(define (compound-unit-fst u)
  (cadr u))

;; 获取复合单位的第二个单位
(define (compound-unit-sec u)
  (caddr u))

;; 寻找操作符对应的单位转换器的组合器
(define (find-operator-composer op)
  (cond
   [(eq? op '/) unit:/]
   [(eq? op 'expt) unit:expt]
   [(eq? op 'invert) unit:invert]
   [else 'none-operator-composer]))



;; 华氏度和摄氏度的关系
(register-unit-conversion 'fahrenheit 'celsius
                          (make-unit-conversion (lambda (f) (* 5/9 (- f 32)))
                                                (lambda (c) (+ (* c 9/5) 32))))

;; 摄氏度与绝对度的关系
(register-unit-conversion 'celsius 'kelvin
                          (let ((zero-celsius 273.15)) ;kelvins
                            (make-unit-conversion
                             (lambda (c) (+ c zero-celsius))
                             (lambda (k) (- k zero-celsius)))))





(define make-specialized-gas-law-volume
  (unit-specializer gas-law-volume
                    '(expt meter 3) ; output (volume)
                    '(/ newton (expt meter 2)) ; pressure
                    'kelvin ; temperature
                    'mole)) ; amount


(define conventional-gas-law-volume
  (make-specialized-gas-law-volume
   '(expt inch 3) ; output (volume)
   '(/ pound (expt inch 2)) ; pressure
   'fahrenheit ; temperature
   'mole)) ; amount


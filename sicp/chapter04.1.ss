(define apply-in-underlying-scheme apply)

(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(assignment? exp) (eval-assignment exp env)]
        [(definition? exp) (eval-definition exp env)]
        [(if? exp) (eval-if exp env)]
        [(lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env)]
        [(begin? exp) (eval-sequence (begin-actions exp) env)]
        [(cond? exp) (eval (cond->if exp) env)]
        [(application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env))]
        [else
         (error 'eval "Unknow expression type -- EVAL" exp)]))


(define (apply procedure arguments)
  (cond [(primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments)]
        [(compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure)))]
        [else
         (error 'apply "Unknow procedure type -- APPLY" procedure)]))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond [(last-exp? exps)
         (eval (first-exp exps) env)]
        [else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env)]))


(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp))
                       env)
  'ok)


(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)


(define (self-evaluating? exp)
  (cond [(number? exp) #t]
        [(string? exp) #t]
        [else #f]))

(define (variable? exp)
  (symbol? exp))


(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))


(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))


(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cdddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))

(define (make-begin seq)
  (cons 'begin seq))


(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))



(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))


(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ([first (car clauses)]
            [rest (cdr clauses)])
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error 'expand-clauses "ELSE clauses isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))




(define (true? x)
  (not (eq? x 'false)))

(define (false? x)
  (eq? x 'false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))



(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())


(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error 'extend-environment "Too many arguments supplied" vars vals)
          (error 'extend-environment "Too few arguments supplied" vars vals))))


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (car vals)]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (error 'lookup-variable-value "Unbound variable" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vals))
             (set-car! vals val)]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (error 'set-variable-value! "Unbound variable" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (define (scan vars vals)
      (cond [(null? vars)
             (add-binding-to-frame! var val frame)]
            [(eq? var (car vars))
             (set-car! vals val)]
            [else
             (scan (cdr vars) (cdr vals))]))
    (scan (frame-variables frame)
          (frame-values frame))))



(define (setup-environment)
  (let ([initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)])
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map
   (lambda (proc) (list 'primitive (cadr proc)))
   primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (let ([output (eval input the-global-environment)])
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input s)
  (newline)
  (newline)
  (display s)
  (newline))

(define (announce-output s)
  (newline)
  (display s)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))


(driver-loop)

(define (append x y) (if (null? x) y (cons (car x) (append (cdr x) y))))
(append '(a b c) '(d e f))

(define (aaa x) x)


(eval '(define (append x y) (if (null? x) y (cons (car x) (append (cdr x) y))))
      the-global-environment)

(eval (list 'append '(a b c) '(d e f))
      the-global-environment)

(eval (list 'cons 1 3) the-global-environment)



;; 练习 4.15
;; 如果try能停下来，它就会执行run-forever，于是会永远运行下去。如果它不能停下来，它就执行另一个分支，直接返回一个值。
;; 如果(halts? try try)返回true，表示能停机，可实际运行时候却并不能停机。这是矛盾点。
;; 换句话说，halts?错误了。





(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))



(define (an-integer-between low high)
  (require (< low high))
  (amb low (an-integer-between (+ low high))))


;; 练习 4.36
(define (a-pythagorean-triple-between low)
  (let ([i (an-integer-starting-from low)])
    (let ([j (an-integer-starting-from i)])
      (let ([k (an-integer-between j (+ i j))])
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
;; 主要是因为k没有上界，k没办法失败，而后返回重试。所以k必须有上界限制。

(define (distinct? ls)
  (cond [(null? ls) #t]
        [(null? (cdr ls)) #t]
        [else (and (let loop ([item (car ls)]
                              [left (cdr ls)])
                     (cond [(null? left) #t]
                           [(eq? item (car left)) #f]
                           [else (loop item (cdr left))]))
                   (distinct? (cdr ls)))]))


(define (multiple-dwelling)
  (let ([baker (amb 1 2 3 4 5)]
        [cooper (amb 1 2 3 4 5)]
        [fletcher (amb 1 2 3 4 5)]
        [miller (amb 1 2 3 4 5)]
        [smith (amb 1 2 3 4 5)])
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)


;; 练习 4.43
(define (lying)
  (let ([bei (amb 1 2 3 4 5)]
        [ai (amb 1 2 3 4 5)]
        [qiong (amb 1 2 3 4 5)]
        [kai (amb 1 2 3 4 5)]
        [ma (amb 1 2 3 4 5)])
    (one-lying (= kai 2) (= bei 3))
    (one-lying (= ai 1) (= qiong 2))
    (one-lying (= qiong 3) (= ai 5))
    (one-lying (= kai 2) (= ma 4))
    (one-lying (= ma 4) (= bei 1))))

(define (one-lying a b)
  (if a (if b (amb)))
  (if (not a) (if (not b) (amb))))
;; 练习 4.43 end




;; 自然语言的语法分析
(define nonus '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

(sentence (noun-phrase (article the) (noun cat))
          (verb eats))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nonus)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ([found-word (car *unparsed*)])
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ([sent (parse-sentence)])
    (require (null? *unparsed*))
    sent))


(define prepositions '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))


(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))


(define (parse-define)
  (list 'define
        (parse-symbol)
        (amb (parse-symbol)
             (parse-number))))


(amb verb-phrase
     (maybe-extend (list 'verb-phrase
                         verb-phrase
                         (parse-prepositional-phrase))))

(amb verb-phrase
     (amb (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase))
          (maybe-extend (list 'verb-phrase
                              (list 'verb-phrase
                                    verb-phrase
                                    (parse-prepositional-phrase))
                              (parse-prepositional-phrase)))))


(amb verb-phrase
     (amb (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase))
          (amb (list 'verb-phrase
                     (list 'verb-phrase
                           verb-phrase
                           (parse-prepositional-phrase))
                     (parse-prepositional-phrase))
               (maybe-extend
                (list 'verb-phrase
                      (list 'verb-phrase
                            (list 'verb-phrase
                                  verb-phrase
                                  (parse-prepositional-phrase))
                            (parse-prepositional-phrase))
                      (parse-prepositional-phrase))))))



;; 创建账户
(define make-account
  (lambda (money)
    (lambda (op)
      (cond [(eq? op 'left-money) money]
            [(eq? op 'take-money)
             (lambda (n)
               (if (<= n money)
                   (make-account (- money n))
                   (error 'take-money "余额不足")))]))))

;; 查看余额
(define left-money
  (lambda (account)
    (account 'left-money)))

;; 取钱
(define take-money
  (lambda (account n)
    ((account 'take-money) n)))



;; 测试代码
(let ([init-account (make-account 100)])
  (let ([acc (take-money init-account 20)])
    (left-money acc)))


(define fail #f)
(define-syntax amb
  (syntax-rules ()
    ((_) (fail))
    ((_ a) a)
    ((_ a b ...)
     (let ((fail0 fail))
       (call/cc
        (lambda (cc)
          (set! fail
            (lambda ()
              (set! fail fail0)
              (cc (amb b ...))))
          (cc a)))))))


(define (parse input)
  (let ([line (read-line input)])
    (if (not (end-line? line))
        (handle line))))





(define (one-distinct a b c d)
  (cond [(and (= b c d) (not (= a b))) 1]
        [(and (= a c d) (not (= a b))) 2]
        [(and (= a b d) (not (= c b))) 3]
        [(and (= a b c) (not (= d b))) 4]
        [else (amb)]))

(define (require-answer q v)
  (if (not (= q v)) (amb)))

(define (run)
  (define q1 (amb 1 2 3 4))
  (define q2 (amb 1 2 3 4))
  (define q3 (amb 1 2 3 4))
  (define q4 (amb 1 2 3 4))
  (define q5 (amb 1 2 3 4))
  (define q6 (amb 1 2 3 4))
  (define q7 (amb 1 2 3 4))
  (define q8 (amb 1 2 3 4))
  (define q9 (amb 1 2 3 4))
  (define q10 (amb 1 2 3 4))

  (amb (and (= q2 1) (= q5 3))
       (and (= q2 2) (= q5 4))
       (and (= q2 3) (= q5 1))
       (and (= q2 4) (= q5 2)))

  (require-answer q3 (one-distinct q3 q6 q2 q4))

  (amb (and (= q4 1) (= q1 q5))
       (and (= q4 2) (= q2 q7))
       (and (= q4 3) (= q1 q9))
       (and (= q4 4) (= q6 q10)))

  (amb (and (= q5 1) (= q8 q5))
       (and (= q5 2) (= q4 q5))
       (and (= q5 3) (= q9 q5))
       (and (= q5 4) (= q7 q5)))

  (amb (and (= q6 1) (= q8 q2 q4))
       (and (= q6 2) (= q8 q1 q6))
       (and (= q6 3) (= q8 q3 q10))
       (and (= q6 4) (= q8 q5 q9)))

  (list q1 q2 q3 q4 q5 q6 q7 q8 q9 q10))


(run)


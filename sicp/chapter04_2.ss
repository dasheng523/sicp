(address (Bitdiddle Ben) (Slumerville (Ridege Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 60000)
(supervisor (Bitdiddle Ben) (Warbucks Oliver))

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer programmer))
(salary (Tweakit Lem E) 25000)
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))

(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)

(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))

(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(job (Aull DeWitt) (administration secretary))
(salary (Aull DeWitt) 25000)
(supervisor (Aull DeWitt) (Warbucks Oliver))


(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))
(can-do-job (computer programmer)
            (computer programmer trainee))
(can-do-job (administration secretary)
            (administration big wheel))


;; 练习 4.55
;; a
(supervisor ?x (Bitdiddle Ben))

;; b
(job ?x (accounting . ?t))

;; c
(address ?x (Slumerville . ?y))


;; 练习 4.56
;; a
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?where))

;; b
(and (or (salary ?person ?sal)
         (salary (Bitdiddle Ben) ?ben-sal))
     (list-value < ?sal ?ben-sal))

;; c
(and (job ?computor (computer . ?x))
     (not (supervisor ?x ?computor)))



(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?x ?x))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))


;; 练习 4.57
(rule (can-replace ?person-1 ?person-2)
      (and
       (or (and (and (job ?person-1 ?job)
                     (can-do-job ?job ?cjob))
                (and (job ?person-2 ?job)
                     (can-do-job ?job ?cjob)))
           (can-replace ?p ?person-1))
       (not (same ?person-1 ?person-2))))


(can-replace (Fect Cy D) ?x)

(and (can-replace ?p1 ?p2)
     (salary ?p1 ?s1)
     (salary ?p2 ?s2)
     (list-value > ?s1 ?s2))

;; 练习4.58
(rule (leader ?person)
      (and (job ?person (?dep . ds-1))
           (job ?x (?dep . ds-2))
           (not (supervisor ?person ?x))))


;; 练习4.59
;; a
(and (meeting ?x (Friday ?t))
     (list-value am? ?t))

;; b
(rule (meeting-time ?person ?day-and-time)
      (and (job ?person (?dep . ds))
           (or (meeting ?dep ?day-and-time)
               (meeting whole-company ?day-and-time))))

;; c
(meeting-time (Hacker Alyssa P) (Wednesday ?t))


;; 练习 4.60
;; 很难做到啊，列出两次是因为程序会把所有情况都尝试一遍，而每个尝试都是独立进行的，是不知道对方在做顺序不一样但相同的情况。
;; 所以只能在返回结果处过滤掉相等的情况吧。
;; 这道题不太会。



(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))




;; 练习 4.61
(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))

(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))


;; 练习 4.62
(rule (last-pair ?a ?a)
      (same ?a (?x . ())))

(rule (last-pair (?a . ?rest) ?e)
      (last-pair ?rest ?e))


;; 练习 4.63
(rule (grandson ?a ?b)
      (and (son ?a ?x)
           (son ?x ?b)))

(rule (son-1 ?a ?b)
      (or (son ?a ?b)
          (and (wift ?a ?w)
               (son ?w ?b))))

;; 练习 4.64
;; 问题点出在and语句。and的第一个语句是一个递归调用，当它不停递归，直到满足or的第一个语句时，查询就得到了第一个答案，随后开始求值or的第二个语句，所以很快又遇到了这个递归，此递归是没有终止条件的，所以陷入了无限循环。


;; 练习 4.65
;; 这里出现重复的次数由中级管理人有多少个直属下属决定的。

;; 合一结论后的框架:
?who => ?person

;; 求值(supervisor ?middle-manager ?person)后的框架序列
?middle-manager => A1
?person => A2
?who => ?person

?middle-manager => B1
?person => B2
?who => ?person

?middle-manager => C1
?person => C2
?who => ?person

;; 求值(supervisor ?x ?middle-manager)后的框架序列

?x => X1
?middle-manager => A1
?person => A2
?who => ?person

?x => X2
?middle-manager => A1
?person => A2
?who => ?person

;; 问题出在这里，当一个中级管理人有多个下属时，将一个框架分裂成多个框架，所以产生的结果就变成了多个。


;; 练习 4.66
;; 既然练习4.65出现了冗余数据，那么Ben简单地用聚合函数汇总其结果肯定也是错误的。
;; 解决这个问题的思路就是，移除冗余的数据，使得数据变得更正确。
;; 比如想要计算所有wheel人物的总工资，那么需要先对输出流的框架进行分组，然后再计算他们的工资。

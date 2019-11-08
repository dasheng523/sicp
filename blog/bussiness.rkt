#lang racket

(define (article-list-page tags page pagesize)
  (iter-list
   (filter-page (find-articles-by-tags tags)
               page
               pagesize)
   'title 'content))


(define (article-info id)
  (find-article-by-id id)
  (find-comments-by-article_id id))

(define (zan-article id)
  )

(define (comment-article id content)
  )

(define (publish-article article)
  (save-or-update-db! article))

(define (delete-article id)
  (delete-db! 'article id))

(define (login name pass)
  (let [(dd (find-user-by-name-pass user pass))]
    (iif dd
         (begin (set-entity dd 'login_time 1)
                (update-db! dd))
         #f)))

(define (register name pass)
  (iif (find-user-by-name user)
         #f
         (save-db! (user-entity name pass))))


;; (define (find title content) 1)
;; (define (find data) 1) data包含title,content
;; 第二种也是可以,但必须规定清楚data的类型。这样做了之后，就跟上面那种区别不大了。
;; 如果不定义类型，data就不明确了，调用者为了知道data是什么，还必须查看定义。

;; 别人是怎么定义呢？思路是什么样的？
;; 如果data类型确定，那么可以使用，但是如果data种类很多，那还是使用第一种比较好。

;; filter-page应该定义到这里吗？
;; 这个跟实现有关系了，目前可以先不管。

;; 到底用一个兼容新增和更新的函数好，还是写成两个好？
;; 都差不多的。
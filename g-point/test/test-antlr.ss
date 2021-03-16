(load "../startup.ss")

(define file (open-input-file "e:/project/test_antlr4/sss.ss"))

(read (open-input-file "e:/project/test_antlr4/sss.ss"))




(define create-translator
  (case-lambda
    [(tag checker handler)
     (lambda (x)
       (record-case x
                    [(name) () tag]
                    [(match?) (lst) (checker lst)]
                    [(handle) (lst) (handler lst)]))]
    [(tag handler)
     (create-translator tag (lambda (lst) (eq? (car lst) tag)) handler)]))

(define (translator-match? translator lst)
  (translator `(match? ,lst)))
(define (translator-handle translator lst)
  (translator `(handle ,lst)))
(define (translator-name translator)
  (translator `(name)))


(define variableStatement-translator
  (create-translator 'variableStatement
                     (lambda (lst)
                       (translate (cadr lst)))))

(define variableDeclarationList-translator
  (create-translator 'variableDeclarationList
                     (lambda (lst)
                       (let ([tran (lambda (lst)
                                     (let ([statement (translate (caddr lst))]
                                           [more (if (null? (cdddr lst)) '() (translate (cddddr lst)))])
                                       (cons statement more)))])
                         `(begin ,@(tran lst))))))



(define variableDeclaration-translator
  (create-translator 'variableDeclaration
                     (lambda (lst)
                       (let ([var-name (translate (cadr lst))]
                             [var-val (translate (cadddr lst))])
                         `(define ,var-name ,var-val)))))

(define assignable-translator
  (create-translator 'assignable
                     (lambda (lst)
                       (translate (cadr lst)))))

(define identifier-translator
  (create-translator 'identifier
                     (lambda (lst)
                       (cadr lst))))

(define singleExpression-translator
  (create-translator 'singleExpression
                     (lambda (lst)
                       (let* ([dlst (cdr lst)]
                              [trans-d (match-translator dlst)])
                         (if trans-d
                             (translator-handle trans-d dlst)
                             (let ([trans-a (match-translator (car dlst))])
                               (if trans-a
                                   (translator-handle trans-a (car dlst))
                                   (error 'translate "找不到匹配的singleExpression翻译器" lst))))))))




(define notExpression-translator
  (create-translator
   'notExpression
   (lambda (lst)
     (and (eq? (car lst) '!) (translator-match? singleExpression-translator (cadr lst))))
   (lambda (lst)
     (let ([expr (translate (cadr lst))])
       `(not ,expr)))))


(define literal-translator
  (create-translator 'literal
                     (lambda (lst)
                       (translate (cadr lst)))))

(define numericLiteral-translator
  (create-translator 'numericLiteral
                     (lambda (lst)
                       (cadr lst))))

(define test-translator
  (create-translator 'test
                     (lambda (lst)
                       (display lst))))

(define get-translators
  (lambda ()
    (list test-translator\
          ariableStatement-translator
          variableDeclarationList-translator
          variableDeclaration-translator
          assignable-translator
          identifier-translator
          singleExpression-translator
          literal-translator
          numericLiteral-translator
          notExpression-translator)))




(define match-translator
  (case-lambda
    [(translators lst)
     (if (or (null? translators) (not (pair? lst)))
         #f
         (if (translator-match? (car translators) lst)
             (car translators)
             (match-translator (cdr translators) lst)))]
    [(lst)
     (match-translator (get-translators) lst)]))

(define translate
  (case-lambda
    [(translators lst)
     (let ([translator (match-translator translators lst)])
       (if translator
           (translator-handle translator lst)
           (error 'translate "找不到匹配的翻译器" lst)))]
    [(lst)
     (let ([trans (get-translators)])
       (translate trans lst))]))


(define lst-match?
  (lambda (lst patterns)
    (syntax-case lst ()
      [(_) #t]
      [(_ e) #t])))

(lst-match? '(a b c)
            (list '("function" "(" ,numericLiteral-translator ")")
                  '("async function" "(" ,numericLiteral-translator ")")))



(translate '(singleExpression ! (singleExpression (identifier a))))
(translate '(literal (numericLiteral 3.141593)))
(translate '(variableDeclaration
             (assignable (identifier PI))
             =
             (singleExpression (literal (numericLiteral 3.141593)))))

(TRANSLATE '(VARIABLEDECLARATIONLIST
             (VARMODIFIER CONST)
             (VARIABLEDECLARATION
              (ASSIGNABLE (IDENTIFIER PI))
              =
              (SINGLEEXPRESSION (LITERAL (NUMERICLITERAL 3.141593))))))





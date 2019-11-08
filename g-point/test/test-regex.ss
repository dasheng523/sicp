(case (machine-type)
  [(i3le ti3le a6le ta6le) (load-shared-object "libc.bll")]
  [(i3osx ti3osx a6osx ta6osx) (load-shared-object "libc.dylib")]
  [else (load-shared-object "libc.so")])

(foreign-entry? "regcomp")

(library-directories "/Users/huangyesheng/Documents/project/main-world/g-point")
(import (ffi ffi-utils))

(define (map-with-index p ls)
  (let iter ([i 0] [p p] [ls ls])
    (cond ((null? ls) '())
          (else (cons (p (car ls) i)
                      (iter (+ i 1) p (cdr ls)))))))


(define-syntax define-enum-ftype
  (lambda (x)
    (syntax-case x ()
      [(_ fname ([ename val] ...) ftype)
       #'(begin
           (define ename val) ...
           (define-ftype fname ftype))]
      [(_ fname (ename ...) ftype)
       #`(define-enum-ftype fname
           [#,@(map-with-index
                (lambda (name index)
                  `(,name ,index))
                #'(ename ...))]
           ftype)])))



(define-enum-ftype reg-errcode-t
  (REG_NOERROR     ;; Success.
   REG_NOMATCH     ;; Didn't find a match (for regexec).
   REG_BADPAT      ;; Invalid pattern.
   REG_ECOLLATE    ;; Invalid collating element.
   REG_ECTYPE      ;; Invalid character class name.
   REG_EESCAPE     ;; Trailing backslash.
   REG_ESUBREG		 ;; Invalid back reference.
   REG_EBRACK		   ;; Unmatched left bracket.
   REG_EPAREN		   ;; Parenthesis imbalance.
   REG_EBRACE		   ;; Unmatched \{.
   REG_BADBR		   ;; Invalid contents of \{\}.
   REG_ERANGE		   ;; Invalid range end.
   REG_ESPACE		   ;; Ran out of memory.
   REG_BADRPT		   ;; No preceding re for repetition op.
   )
  int)


(define-enum-ftype cflag-t
  (#| If this bit is set, then use extended regular expression syntax.
   If not set, then use basic regular expression syntax.  |#
   [REG_EXTENDED 1]

   #| If this bit is set, then ignore case when matching.
   If not set, then case is significant.   |#
   [REG_ICASE (bitwise-arithmetic-shift-left 1 1)]

   #| If this bit is set, then anchors do not match at newline
   (regexec regex-entry "hello42world" 100 matchdata 0)characters in the string.
   If not set, then anchors do match at newlines. |#
   [REG_NEWLINE (bitwise-arithmetic-shift-left 1 2)]

   #|
   If this bit is set, then report only success or fail in regexec.
   If not set, then returns differ between not matching and errors.  |#
   [REG_NOSUB (bitwise-arithmetic-shift-left 1 3)])
  int)


(define-enum-ftype eflag-t
  (#| If this bit is set, then the beginning-of-line operator doesn't match
   the beginning of the string (presumably because it's not the
   beginning of a line).
   If not set, then the beginning-of-line operator does match the
   beginning of the string.   |#
   [REG_NOTBOL 1]

   #| Like REG_NOTBOL, except for the end-of-line.  |#
   [REG_NOTEOL (bitwise-arithmetic-shift-left 1 1)]

   #| Use PMATCH[0] to delimit the start and end of the search in the buffer. |#
   [REG_STARTEND (bitwise-arithmetic-shift-left 1 2)]
  )
  int)

(define-ftype regex-t (struct))

(define-ftype regmatch-t
  (struct [rm_so ssize_t] [rm_eo ssize_t]))

(define-ftype regmatch-array-t
  (array 100 regmatch-t))

(define regcomp
  (foreign-procedure
   "regcomp"
   ((* regex-t) string cflag-t)
   reg-errcode-t))


(define regexec
  (foreign-procedure
   "regexec"
   ((* regex-t) string int (* regmatch-array-t) eflag-t)
   reg-errcode-t))

(define regfree
  (foreign-procedure
   "regfree"
   ((* regex-t))
   void))

;; (foreign-sizeof type)
;; (foreign-alloc n)
;; (foreign-free address)
;; (ftype-pointer-address)
;; (ftype-sizeof)


(define regex-entry (make-ftype-pointer
                     regex-t
                     (foreign-alloc 100)))

(foreign-free (ftype-pointer-address regex-entry))

(regcomp regex-entry "hello([0-9]*)world" REG_EXTENDED)


(define matchdata (make-ftype-pointer
                   regmatch-array-t
                   (foreign-alloc (ftype-sizeof regmatch-array-t))))
(define simdata (make-ftype-pointer
                 regmatch-t
                 (foreign-alloc (ftype-sizeof regmatch-t))))

matchdata

(regexec regex-entry "hello42world" 100 matchdata 0)

(ftype-ref regmatch-array-t (0 rm_so) matchdata)
(ftype-ref regmatch-array-t (0 rm_eo) matchdata)
(ftype-ref regmatch-array-t (1 rm_so) matchdata)
(ftype-ref regmatch-array-t (1 rm_eo) matchdata)
(ftype-ref regmatch-array-t (3 rm_eo) matchdata)

(ftype-ref regmatch-t (rm_eo) simdata)



(define-ftype B
  (struct
    [b1 integer-32]
    [b2 (array 10 integer-32)]))

(define-ftype C (* B))

(library-directories "/Users/huangyesheng/Documents/project/main-world/g-point")

(import (lib regex))


(error reg-find "ddd")

(raise "ddd")

(library (ffi regex)
  (export reg-errcode-t
          cflag-t
          eflag-t
          regex-t
          regmatch-t
          regmatch-array-t
          init-regex-t init-regmatch-array-t regmatch-array->list

          regcomp regexec regfree regerror)
  (import (chezscheme)
          (ffi ffi-utils))


  (define _init
    (load-shared-object (string-append "libc" (machine-lib-suffix))))


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


  ;; 初始化regex-t数据结构
  (define (init-regex-t)
    (make-ftype-pointer regex-t (foreign-alloc 100)))

  ;; 初始化regmatch数据结构
  (define (init-regmatch-array-t)
    (make-ftype-pointer
     regmatch-array-t
     (foreign-alloc (ftype-sizeof regmatch-array-t))))

  ;; 将regmatch转换为list
  (define (regmatch-array->list regarr)
    (define (iter i)
      (if (= -1 (ftype-ref regmatch-array-t (i rm_so) regarr))
          '()
          (cons (cons (ftype-ref regmatch-array-t (i rm_so) regarr)
                      (ftype-ref regmatch-array-t (i rm_eo) regarr))
                (iter (+ i 1)))))
    (iter 0))


  ;; 编译正则对象
  (define regcomp
    (foreign-procedure
     "regcomp"
     ((* regex-t) string cflag-t)
     reg-errcode-t))

  ;; 执行正则匹配
  (define regexec
    (foreign-procedure
     "regexec"
     ((* regex-t) string int (* regmatch-array-t) eflag-t)
     reg-errcode-t))

  ;; 释放正则对象
  (define regfree
    (foreign-procedure
     "regfree"
     ((* regex-t))
     void))

  ;; 获取正则错误
  (define regerror
    (foreign-procedure
     "regerror"
     (int (* regex-t) string size_t)
     int))


  #|
  (define regcomp
    (foreign-procedure
     "regcomp"
     ((* regex-t) string int)
     int))
  (define regexec
    (foreign-procedure
     "regexec"
     ((* regex-t) string int (* regmatch-array-t) int)
     int))
  (define aa (init-regex-t))

  (define-ftype regmatch-t
    (struct [rm_eo ssize_t]))
  (define-ftype regmatch-array-t
    (array 100 regmatch-t))

  (regcomp aa "aa" 1)

  (define rs (make-ftype-pointer regmatch-array-t (foreign-alloc 1)))
  (regexec aa "bbaacc" 3 rs 0)

  (ftype-ref regmatch-array-t (2 rm_eo) rs)
  |#

  )

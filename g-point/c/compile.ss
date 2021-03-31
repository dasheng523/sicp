(library (c compile)
  (export compile-on-linux
          compile-on-solaris
          compile-on-windows
          compile-on-mac)
  (import (chezscheme)
          (lib common)
          (common combinator))



  (define (compile-command-on-linux source-list target-name)
    (assert (pair? source-list))
    (string-append "cc -fPIC -shared -m64 -o "
                   target-name
                   " "
                   (string-join source-list " ")))

  (define (compile-command-on-mac source-list target-name)
    (assert (pair? source-list))
    (string-append "cc -dynamiclib -m64 -o "
                   target-name
                   " "
                   (string-join source-list " ")))

  (define (compile-command-on-windows source-list target-name)
    (assert (pair? source-list))
    (let ([comlist (map (lambda (s) (string-append "cl -c -DWIN32 " s))
                      source-list)]
          [com (string-append
                "link -dll -out:"
                target-name
                " "
                (string-join
                 (map (lambda (x)
                        (substring x (- (string-length x) 2)
                                   (string-length x)))
                      source-list)
                 " "))])
      (apply command-mix (append comlist (list com)))))


  ;; 将多个命令合并成一个
  (define (command-mix . commands)
    (assert (> (length commands) 0))
    (string-join " & " commands))

  (define (choose-command key)
    (cond
     [(eq? key 'windows) compile-command-on-windows]
     [(eq? key 'solaris) compile-command-on-solaris]
     [(eq? key 'mac) compile-command-on-mac]
     [else compile-command-on-linux]))


  ;; On Linux, FreeBSD, OpenBSD, and OpenSolaris systems x64
  (define compile-on-linux
    (compose system compile-command-on-linux))

  ;; On MacOS X (Intel or PowerPC) systems x64:
  (define compile-on-mac
    (compose system compile-command-on-mac))


  ;; On Windows
  (define compile-on-windows
    (compose system compile-command-on-windows))


  )

(library (c compile)
  (export compile-on-linux compile-on-solaris compile-on-windows compile-on-mac)
  (import (chezscheme)
          (lib common))

  ;; On Linux, FreeBSD, OpenBSD, and OpenSolaris systems x64
  (define (compile-on-linux source-list target-name)
    (let ([shell (string-append "cc -fPIC -shared -m64 -o "
                                target-name
                                " "
                                (string-join source-list " "))])
      (system shell)))

  ;; On MacOS X (Intel or PowerPC) systems x64:
  (define (compile-on-mac source-list target-name)
    (let ([shell (string-append "cc -dynamiclib -m64 -o "
                                target-name
                                " "
                                (string-join source-list " "))])
      (system shell)))


  ;; On 64-bit Sparc Solaris:
  (define (compile-on-solaris source-list target-name)
    (let ([shell (string-append "cc -xarch=v9 -KPIC -G -o "
                                target-name
                                " "
                                (string-join source-list " "))])
      (system shell)))

  ;; On Windows
  (define (compile-on-windows source-list target-name)
    (let iter ([s source-list])
      (system (string-append "cl -c -DWIN32 " s)))
    (system (string-append
             "link -dll -out:"
             target-name
             " "
             (string-join
              (map (lambda (x)
                     (substring x (- (string-length x) 2)
                                (string-length x)))
                   source-list)
              " "))))
  )

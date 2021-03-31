(library (common compile)
  (export the-system
          the-bit
          the-threaded?
          compile-thirdpart-file
          thirdpart-source-dire
          thirdpart-lib-dire
          current-system-suffix)
  (import (chezscheme)
          (common combinator)
          (common utils)
          (lib common))

  ;; 获取当前系统
  (define the-system
    (compose platform-system ->string machine-type))

  ;; 获取当前scheme的位数
  (define the-bit
    (compose platform-bit ->string machine-type))

  ;; 获取当前scheme是否是安全线程
  (define the-threaded?
    (compose platform-threaded? ->string machine-type))




  ;; 在Linux下的编译命令
  (define (compile-command-on-linux source-list target-name)
    (assert (pair? source-list))
    (string-append "cc -fPIC -shared -m64 -o "
                   target-name
                   " "
                   (string-join source-list " ")))

  ;; 在osx下的编译命令
  (define (compile-command-on-osx source-list target-name)
    (assert (pair? source-list))
    (string-append "cc -dynamiclib -m64 -o "
                   target-name
                   " "
                   (string-join source-list " ")))

  ;; 在windows下的编译命令
  ;; "gcc --shared -o mongoose.dll mongoose.c -lws2_32"
  (define (compile-command-on-windows source-list target-name)
    (assert (pair? source-list))
    (string-append "gcc --shared -o "
                   target-name
                   " "
                   (string-join source-list " ")
                   " -lws2_32"))




  (define system-command-table
    (list (cons 'windows compile-command-on-windows)
          (cons 'linux compile-command-on-linux)
          (cons 'osx compile-command-on-osx)))

  (define system-suffix-table
    (list (cons 'windows ".dll")
          (cons 'linux ".so")
          (cons 'osx ".dylib")))

  ;; 选择编译命令
  (define choose-command
    (match-table system-command-table eq?))

  ;; 根据操作系统选择后缀
  (define choose-suffix
    (match-table system-suffix-table eq?))

  ;; 根据当前系统获取动态库后缀
  (define current-system-suffix
    (compose choose-suffix the-system))


  ;; 编译文件
  (define compile-c-file
    (compose system (choose-command (the-system))))




  ;; 第三方库源码的位置
  (define thirdpart-source-dire
    (string-append (current-project-dire) "/thirdpart"))

  ;; 第三方库编译文件的位置
  (define thirdpart-lib-dire
    (string-append (current-project-dire) "/thirdpart/lib"))


  ;;(adapt-thirdpart-path (list "mongoose.h" "mongoose.c") "mongoose.dll")
  (define (adapt-thirdpart-path source-list target-name)
    (define prefix-path
      (lambda (x)
        (string-append thirdpart-source-dire "/" x)))
    (define lib-path
      (lambda (x)
        (string-append thirdpart-lib-dire "/" x)))
    (values
     (map prefix-path source-list)
     (lib-path target-name)))

  ;; 编译第三方文件
  ;;(compile-thirdpart-file (list "mongoose.c") "mongoose.dll")
  (define compile-thirdpart-file
    (compose compile-c-file adapt-thirdpart-path))

  )


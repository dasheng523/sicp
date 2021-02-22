(library (env config)
  (import (chezscheme))
  (export get-val load-config)


  ;; 读公共环境配置
  ;; 读对应环境的内容
  ;; 合并这两个配置
  (define (get-val c k)
    (assq c k))

  (define (load-config)
    '((lib-dire . "e:/project/sicp/g-point")
      (lib-ext . ".so")))


  )

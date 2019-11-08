(library (env config)
  (import (chezscheme))
  (export config load-config)

  (define state '())

  ;; 读公共环境配置
  ;; 读对应环境的内容
  ;; 合并这两个配置
  (define (config k)
    (assq state k))

  (define (load-config)
    (load-public-config)
    (load-env-config))

  (define (load-public-config)
    )

  )

;; 导入头文件，返回环境
(define env (include-h-file "xxx.h"))

;; function-instance 实例化C函数
(define connect (function-instance env 'connect))

;; object-instance 可以直接传类型名
(define db (object-instance env 'mysql))

;; object-instance 也可以传类型名和初始值
(define db (object-instance env 'mysql '((a . 11) (b . 66))))

;; value-getter 可获得环境中的数据。这个函数可以不用。
(define mysql-version (value-getter env 'version))

;; 调用这个函数，相当于调用C函数
(c-apply connect "localhost" "user" "passwd" db)

;; 能从db里取出数据
(data-extractor db)



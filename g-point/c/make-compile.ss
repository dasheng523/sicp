(import (c compile)
        (ffi ffi-utils))


(compile-on-linux (list (string-append c-bin-dire "/mongoose.c"))
                  (string-append c-bin-dire "/mongoose" (machine-lib-suffix)))

(compile-on-linux (list (string-append c-bin-dire "/mongoose-utils.c"))
                  (string-append c-bin-dire "/mongoose-utils" (machine-lib-suffix)))



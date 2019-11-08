(library (ffi encrypt)
  (export sha1-ffi SHA_DIGEST_LENGTH
          md5-ffi MD5_DIGEST_LENGTH)
  (import (ffi ffi-utils)
          (chezscheme))


  (define _init
    (load-shared-object (string-append "libssl" (machine-lib-suffix))))

  (define sha1-ffi
    (foreign-procedure
     "SHA1"
     (string int u8*)
     int))

  (define md5-ffi
    (foreign-procedure
     "MD5"
     (string int u8*)
     int))

  (define SHA_DIGEST_LENGTH 20)

  (define MD5_DIGEST_LENGTH 16)


  )

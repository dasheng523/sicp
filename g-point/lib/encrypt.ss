(library (lib encrypt)
  (export sha1 md5)
  (import (ffi encrypt)
          (chezscheme))


  (define (sha1 str)
    (let ([buff (make-bytevector SHA_DIGEST_LENGTH)])
      (sha1-ffi str (string-length str) buff)
      (let loop ([i 0] [rs ""])
        (if (>= i SHA_DIGEST_LENGTH)
            rs
            (let ([data (bytevector-u8-ref buff i)])
              (loop (+ i 1)
                    (string-append rs (format "~2,'0x" data))))))))

  (define (md5 str)
    (let ([buff (make-bytevector MD5_DIGEST_LENGTH)])
      (md5-ffi str (string-length str) buff)
      (let loop ([i 0] [rs ""])
        (if (>= i MD5_DIGEST_LENGTH)
            rs
            (let ([data (bytevector-u8-ref buff i)])
              (loop (+ i 1)
                    (string-append rs (format "~2,'0x" data))))))))


  )

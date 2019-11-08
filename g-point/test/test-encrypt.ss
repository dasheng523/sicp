(library-directories "/Users/huangyesheng/Documents/project/main-world/g-point")
(import (ffi ffi-utils))


(load-shared-object (string-append "libssl" (machine-lib-suffix)))


(define sha1
  (foreign-procedure
   "SHA1"
   (string int u8*)
   int))

(define md5
  (foreign-procedure
   "MD5"
   (string int u8*)
   int))

(define SHA_DIGEST_LENGTH 20)

(define MD5_DIGEST_LENGTH 16)



(define (sha1-test str)
  (let ([buff (make-bytevector SHA_DIGEST_LENGTH)])
    (sha1 str (string-length str) buff)
    (let loop ([i 0] [rs ""])
      (if (>= i SHA_DIGEST_LENGTH)
          rs
          (let ([data (bytevector-u8-ref buff i)])
            (loop (+ i 1)
                  (string-append rs (format "~2,'0x" data))))))))

(define (md5-test str)
  (let ([buff (make-bytevector MD5_DIGEST_LENGTH)])
    (md5 str (string-length str) buff)
    (let loop ([i 0] [rs ""])
      (if (>= i MD5_DIGEST_LENGTH)
          rs
          (let ([data (bytevector-u8-ref buff i)])
            (loop (+ i 1)
                  (string-append rs (format "~2,'0x" data))))))))


(md5-test "523")


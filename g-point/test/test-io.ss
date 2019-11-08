(define ip (open-input-string "this is the input"))
(define op (open-output-string))
(define p (make-two-way-port ip op))

(read p)
(write 'hello p)
(get-output-string op)

(define make-two-way-port
  (lambda (ip op)
    (define handler
      (lambda (msg . args)
        (record-case (cons msg args)
                     [block-read (p s n) (block-read ip s n)]
                     [block-write (p s n) (block-write op s n)]
                     [char-ready? (p) (char-ready? ip)]
                     [clear-input-port (p) (clear-input-port ip)]
                     [clear-output-port (p) (clear-output-port op)]
                     [close-port (p) (mark-port-closed! p)]
                     [flush-output-port (p) (flush-output-port op)]
                     [file-position (p . pos) (apply file-position ip pos)]
                     [file-length (p) (file-length ip)]
                     [peek-char (p) (peek-char ip)]
                     [port-name (p) "two-way"]
                     [read-char (p) (read-char ip)]
                     [unread-char (c p) (unread-char c ip)]
                     [write-char (c p) (write-char c op)]
                     [else (assertion-violationf 'two-way-port
                                                 "operation ~s not handled"
                                                 msg)])))
    (make-input/output-port handler "" "")))




(define make-transcript-port
  (lambda (ip op tp)
    (define (handler msg . args)
      (record-case (cons msg args)
        [block-read (p str cnt)
         (with-interrupts-disabled
           (let ([b (port-input-buffer p)]
                 [i (port-input-index p)]
                 [s (port-input-size p)])
             (if (< i s)
                 (let ([cnt (fxmin cnt (fx- s i))])
                   (do ([i i (fx+ i 1)]
                        [j 0 (fx+ j 1)])
                       ((fx= j cnt)
                        (set-port-input-index! p i)
                        cnt)
                       (string-set! str j (string-ref b i))))
                 (let ([cnt (block-read ip str cnt)])
                   (unless (eof-object? cnt)
                     (block-write tp str cnt))
                   cnt))))]
        [char-ready? (p)
         (or (< (port-input-index p) (port-input-size p))
             (char-ready? ip))]
        [clear-input-port (p)
         ; set size to zero rather than index to size
         ; in order to invalidate unread-char
         (set-port-input-size! p 0)]
        [clear-output-port (p)
         (set-port-output-index! p 0)]
        [close-port (p)
         (with-interrupts-disabled
           (flush-output-port p)
           (set-port-output-size! p 0)
           (set-port-input-size! p 0)
           (mark-port-closed! p))]
        [file-position (p . pos)
         (if (null? pos)
             (most-negative-fixnum)
             (assertion-violationf 'transcript-port "cannot reposition"))]
        [flush-output-port (p)
         (with-interrupts-disabled
           (let ([b (port-output-buffer p)]
                 [i (port-output-index p)])
             (unless (fx= i 0)
               (block-write op b i)
               (block-write tp b i)
               (set-port-output-index! p 0)
               (set-port-bol! p
                 (char=? (string-ref b (fx- i 1)) #\newline))))
           (flush-output-port op)
           (flush-output-port tp))]
        [peek-char (p)
         (with-interrupts-disabled
           (let ([b (port-input-buffer p)]
                 [i (port-input-index p)]
                 [s (port-input-size p)])
             (if (fx< i s)
                 (string-ref b i)
                 (begin
                   (flush-output-port p)
                   (let ([s (block-read ip b)])
                     (if (eof-object? s)
                         s
                         (begin
                           (block-write tp b s)
                           (set-port-input-size! p s)
                           (string-ref b 0))))))))]
        [port-name (p) "transcript"]
        [constituent-ports (p) (values ip op tp)]
        [read-char (p)
         (with-interrupts-disabled
           (let ([c (peek-char p)])
             (unless (eof-object? c)
               (set-port-input-index! p
                 (fx+ (port-input-index p) 1)))
             c))]
        [unread-char (c p)
         (with-interrupts-disabled
           (let ([b (port-input-buffer p)]
                 [i (port-input-index p)]
                 [s (port-input-size p)])
             (when (fx= i 0)
               (assertion-violationf 'unread-char
                 "tried to unread too far on ~s"
                 p))
             (set-port-input-index! p (fx- i 1))
             ; following could be skipped; it's supposed
             ; to be the same character anyway
             (string-set! b (fx- i 1) c)))]
        [write-char (c p)
         (with-interrupts-disabled
           (let ([b (port-output-buffer p)]
                 [i (port-output-index p)]
                 [s (port-output-size p)])
             (string-set! b i c)
            ; could check here to be sure that we really
            ; need to flush; we may end up here even if
            ; the buffer isn't full
             (block-write op b (fx+ i 1))
             (block-write tp b (fx+ i 1))
             (set-port-output-index! p 0)
             (set-port-bol! p (char=? c #\newline))))]
        [block-write (p str cnt)
         (with-interrupts-disabled
          ; flush buffered data
           (let ([b (port-output-buffer p)]
                 [i (port-output-index p)])
             (unless (fx= i 0)
               (block-write op b i)
               (block-write tp b i)
               (set-port-output-index! p 0)
               (set-port-bol! p (char=? (string-ref b (fx- i 1)) #\newline))))
          ; write new data
           (unless (fx= cnt 0)
             (block-write op str cnt)
             (block-write tp str cnt)
             (set-port-bol! p
               (char=? (string-ref str (fx- cnt 1)) #\newline))))]
        [else (assertion-violationf 'transcript-port
                "operation ~s not handled"
                msg)]))
    (let ([ib (make-string 1024)] [ob (make-string 1024)])
      (let ([p (make-input/output-port handler ib ob)])
        (set-port-input-size! p 0)
        (set-port-output-size! p (fx- (string-length ob) 1))
        p))))

(library (ffi socket)
  (export socket_types address-families)
  (import (chezscheme)
          (ffi ffi-utils))

  #;(library-directories "/Users/huangyesheng/Documents/project/main-world/g-point")

  (define _init
    (begin (load-shared-object (string-append "libc" (machine-lib-suffix)))))


  (define-enum-ftype socket_types
    ([SOCK_STREAM     1]
     [SOCK_DGRAM      2]
     [SOCK_RAW        3]
     [SOCK_RDM        4]
     [SOCK_SEQPACKET  5])
    int)

  (define-enum-ftype address-families
    ([AF_UNSPEC       0]
     [AF_UNIX         1]
     [AF_INET         2]
     [AF_IMPLINK      3]
     [AF_PUP          4]
     [AF_CHAOS        5]
     [AF_NS           6]
     [AF_ISO          7]
     [AF_OSI          AF_ISO]
     [AF_ECMA         8]
     [AF_DATAKIT      9]
     [AF_CCITT        10]
     [AF_SNA          11]
     [AF_DECnet       12]
     [AF_DLI          13]
     [AF_LAT          14]
     [AF_HYLINK       15]
     [AF_APPLETALK    16]
     [AF_ROUTE        17]
     [AF_LINK         18]
     [pseudo_AF_XTP   19]
     [AF_COIP         20]
     [AF_CNT          21]
     [pseudo_AF_RTIP  22]
     [AF_IPX          23]
     [AF_SIP          24]
     [pseudo_AF_PIP   25]
     [AF_NDRV         27]
     [AF_ISDN         28]
     [AF_E164         AF_ISDN]
     [pseudo_AF_KEY   29]
     [AF_INET6        30]
     [AF_NATM         31]
     [AF_SYSTEM       32]
     [AF_NETBIOS      33]
     [AF_PPP          34]
     [pseudo_AF_HDRCMPLT 35]
     [AF_RESERVED_36  36]
     [AF_IEEE80211    37]
     [AF_UTUN         38]
     [AF_MAX          40])
    int)

  (define-enum-ftype protocol-families
    ([PF_INET         AF_INET]
     [PF_INET6        AF_INET6])
    int)

  (define-ftype sockaddr
    (struct
      [sa_len unsigned-8]
      [sa_family short]
      [sa_data (array 14 char)]))

  (define-ftype in_addr
    (struct [s_addr unsigned-32]))


  (define-ftype sockaddr_in
    (struct
      [sin_len unsigned-8]
      [sin_family short]
      [sin_port unsigned-16]
      [sin_addr (* in_addr)]
      [sin_zero (array 8 char)]))


  (define bind
    (foreign-procedure "bind" (int (* sockaddr) int)
                       int))

  (define getsockname
    (foreign-procedure "getsockname" (int (* sockaddr) (* int))
                       int))

  (define sockaddr_in
    (c-struct "sockaddr_in"))

  (c-struct-ref sockaddr_in ('sin_len) obj)

  ;; 只要我知道如何从一个struct的指针里读取和写入数据，就可以了。


  )
r

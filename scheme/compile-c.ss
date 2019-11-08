;; eg.
;; On Linux, FreeBSD, OpenBSD, and OpenSolaris systems
;; Depending on the host configuration, the -m32 or -m64 option might be needed to specify 32-bit or 64-bit compilation as appropriate.
(system "cc -fPIC -shared -o evenodd.so even.c odd.c")


;; On MacOS X (Intel or PowerPC) systems:
;; Depending on the host configuration, the -m32 or -m64 option might be needed to specify 32-bit or 64-bit compilation as appropriate.
(system "cc -dynamiclib -o evenodd.so even.c odd.c")


;; On 32-bit Sparc Solaris:
(system "cc -KPIC -G -o evenodd.so even.c odd.c")
;; On 64-bit Sparc Solaris:
(system "cc -xarch=v9 -KPIC -G -o evenodd.so even.c odd.c")


;; On Windows
(system "cl -c -DWIN32 even.c")
(system "cl -c -DWIN32 odd.c")
(system "link -dll -out:evenodd.so even.obj odd.obj")



;; socket
(system "cc -dynamiclib -o ./c/csocket.so ./c/csocket.c")

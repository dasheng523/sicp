(load "../startup.ss")
(import (lib mysql))

(define conn (conn-db "139.9.93.238" "root" "46209084fa6002e2" "dev_platform" 3306))

(close-db conn)

(query-db conn "select * from zc_enterprise_info limit 10")

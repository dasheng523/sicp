(library (ffi mysql)
  (export MYSQL MYSQL_FIELD MYSQL_FIELDS MYSQL_DATA MYSQL_ROW
          MYSQL_ROWS MYSQL_METHODS MEM_ROOT MYSQL_RES
          mysql-init mysql-autocommit mysql-change-user
          mysql-character-set-name mysql-close mysql-commit
          mysql-real-connect mysql-query mysql-store-result
          mysql-num-fields mysql-fetch-row mysql-free-result
          mysql-fetch-fields mysql-ping mysql-error mysql-real-escape-string)
  (import (scheme)
          (ffi ffi-utils))

  (define _init
    (begin
      (load-shared-object
       (string-append "libmysqlclient" (machine-lib-suffix)))))

  (define-ftype MYSQL (struct))

  (define-ftype MYSQL_FIELD
    (struct
        [name (* char)]
      [org_name (* char)]
      [table (* char)]
      [org_table (* char)]
      [db (* char)]
      [catalog (* char)]
      [def (* char)]
      [length unsigned-long]
      [max_length unsigned-long]
      [name_length unsigned-int]
      [org_name_length unsigned-int]
      [table_length unsigned-int]
      [org_table_length unsigned-int]
      [db_length unsigned-int]
      [catalog_length unsigned-int]
      [def_length unsigned-int]
      [flags unsigned-int]
      [decimals unsigned-int]
      [charsetnr unsigned-int]
      [type int]
      [extension void*]))


  (define-ftype MYSQL_FIELDS
    (array 500 MYSQL_FIELD))


  (define-ftype MYSQL_DATA (struct))

  (define-ftype MYSQL_ROW
    (array 500 (* char)))


  (define-ftype MYSQL_ROWS
    (struct))

  (define-ftype MYSQL_METHODS
    (struct))

  (define-ftype MEM_ROOT
    (struct))

  (define-ftype MYSQL_RES
    (struct))


  (define mysql-init
    (foreign-procedure "mysql_init"
                       (void*)
                       (* MYSQL)))

  (define mysql-autocommit
    (foreign-procedure "mysql_autocommit"
                       ((* MYSQL) boolean)
                       boolean))

  ;; bool mysql_change_user(MYSQL *mysql, const char *user, const char *password, const char *db)
  (define mysql-change-user
    (foreign-procedure "mysql_change_user"
                       ((* MYSQL) string string string)
                       boolean))

  ;; Returns the default character set name for the current connection.
  ;; const char *mysql_character_set_name(MYSQL *mysql)
  (define mysql-character-set-name
    (foreign-procedure "mysql_character_set_name"
                       ((* MYSQL) string string string)
                       string))


  ;; mysql_close(): Closes a server connection.
  ;; void mysql_close(MYSQL *mysql)
  (define mysql-close
    (foreign-procedure "mysql_close"
                       ((* MYSQL))
                       void))

  ;; mysql_commit(): Commits the transaction.
  ;; bool mysql_commit(MYSQL *mysql)
  (define mysql-commit
    (foreign-procedure "mysql_commit"
                       ((* MYSQL))
                       boolean))

  #|
  mysql_real_connect(): Connects to a MySQL server.
  MYSQL *mysql_real_connect(MYSQL *mysql, const char *host, const char *user, const char *passwd, const char *db, unsigned int port, const char *unix_socket, unsigned long client_flag)
  |#
  (define mysql-real-connect
    (foreign-procedure
     "mysql_real_connect"
     ((* MYSQL) string string string string unsigned-32 string unsigned-long)
     (* MYSQL)))

  #|
  mysql_query(): Executes an SQL query specified as a null-terminated string.
  int STDCALL mysql_query	(MYSQL * mysql, const char * q)
  |#
  (define mysql-query
    (foreign-procedure
     "mysql_query"
     ((* MYSQL) string)
     int))


  #|
  mysql_store_result(): Retrieves a complete result set to the client.
  MYSQL_RES *mysql_store_result(MYSQL *mysql)
  |#
  (define mysql-store-result
    (foreign-procedure
     "mysql_store_result"
     ((* MYSQL))
     (* MYSQL_RES)))

  #|
  mysql_num_fields(): Returns the number of columns in a result set.
  unsigned int mysql_num_fields(MYSQL_RES *result)
  |#
  (define mysql-num-fields
    (foreign-procedure
     "mysql_num_fields"
     ((* MYSQL_RES))
     unsigned-32))

  #|
  mysql_fetch_row(): Fetches the next row from the result set.
  MYSQL_ROW mysql_fetch_row(MYSQL_RES *result)
  |#
  (define mysql-fetch-row
    (foreign-procedure
     "mysql_fetch_row"
     ((* MYSQL_RES))
     (* MYSQL_ROW)))

  #|
  mysql_free_result(): Frees memory used by a result set.
  void mysql_free_result(MYSQL_RES *result)
  |#
  (define mysql-free-result
    (foreign-procedure
     "mysql_free_result"
     ((* MYSQL_RES))
     void))

  #|
  mysql_fetch_fields(): Returns an array of all field structures.
  MYSQL_FIELD *mysql_fetch_fields(MYSQL_RES *result)
  |#
  (define mysql-fetch-fields
    (foreign-procedure
     "mysql_fetch_fields"
     ((* MYSQL_RES))
     (* MYSQL_FIELDS)))

  #|
  mysql_ping(): Checks whether the connection to the server is working, reconnecting as necessary.
  int mysql_ping(MYSQL *mysql)
  |#
  (define mysql-ping
    (foreign-procedure
     "mysql_ping"
     ((* MYSQL))
     int))


  #|
  mysql_error(): Returns the error message for the most recently invoked MySQL function.
  const char *mysql_error(MYSQL *mysql)
  |#
  (define mysql-error
    (foreign-procedure
     "mysql_error"
     ((* MYSQL))
     string))

  #|
  mysql_real_escape_string(): Escapes special characters in a string for use in an SQL statement, taking into account the current character set of the connection.
  unsigned long mysql_real_escape_string(MYSQL *mysql, char *to, const char *from, unsigned long length)
  |#
  (define mysql-real-escape-string
    (foreign-procedure
     "mysql_real_escape_string"
     ((* MYSQL) (* char) string unsigned-long)
     unsigned-long))

  )

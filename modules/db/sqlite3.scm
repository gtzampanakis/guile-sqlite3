(define-module (db sqlite3))

(use-modules (system foreign))
(use-modules (system foreign-library))
(use-modules (rnrs bytevectors))
(use-modules (ice-9 iconv))

(export
  sqlite3-open
  sqlite3-close
  sqlite3-execute)

; SQLITE_* constants copied from sqlite3.h
(define SQLITE_OK           0)   ; Successful result
(define SQLITE_ERROR        1)   ; Generic error
(define SQLITE_INTERNAL     2)   ; Internal logic error in SQLite
(define SQLITE_PERM         3)   ; Access permission denied
(define SQLITE_ABORT        4)   ; Callback routine requested an abort
(define SQLITE_BUSY         5)   ; The database file is locked
(define SQLITE_LOCKED       6)   ; A table in the database is locked
(define SQLITE_NOMEM        7)   ; A malloc() failed
(define SQLITE_READONLY     8)   ; Attempt to write a readonly database
(define SQLITE_INTERRUPT    9)   ; Operation terminated by sqlite3_interrupt(
(define SQLITE_IOERR       10)   ; Some kind of disk I/O error occurred
(define SQLITE_CORRUPT     11)   ; The database disk image is malformed
(define SQLITE_NOTFOUND    12)   ; Unknown opcode in sqlite3_file_control()
(define SQLITE_FULL        13)   ; Insertion failed because database is full
(define SQLITE_CANTOPEN    14)   ; Unable to open the database file
(define SQLITE_PROTOCOL    15)   ; Database lock protocol error
(define SQLITE_EMPTY       16)   ; Internal use only
(define SQLITE_SCHEMA      17)   ; The database schema changed
(define SQLITE_TOOBIG      18)   ; String or BLOB exceeds size limit
(define SQLITE_CONSTRAINT  19)   ; Abort due to constraint violation
(define SQLITE_MISMATCH    20)   ; Data type mismatch
(define SQLITE_MISUSE      21)   ; Library used incorrectly
(define SQLITE_NOLFS       22)   ; Uses OS features not supported on host
(define SQLITE_AUTH        23)   ; Authorization denied
(define SQLITE_FORMAT      24)   ; Not used
(define SQLITE_RANGE       25)   ; 2nd parameter to sqlite3_bind out of range
(define SQLITE_NOTADB      26)   ; File opened that is not a database file
(define SQLITE_NOTICE      27)   ; Notifications from sqlite3_log()
(define SQLITE_WARNING     28)   ; Warnings from sqlite3_log()
(define SQLITE_ROW         100)  ; sqlite3_step() has another row ready
(define SQLITE_DONE        101)  ; sqlite3_step() has finished executing

(define SQLITE_INTEGER  1)
(define SQLITE_FLOAT    2)
(define SQLITE_TEXT     3)
(define SQLITE_BLOB     4)
(define SQLITE_NULL     5)

(define pointer-size (sizeof '*))

(define foreign-sqlite3-open
  (foreign-library-function
    "libsqlite3" "sqlite3_open"
    #:return-type int
    #:arg-types (list '* '*)))

(define foreign-sqlite3-close
  (foreign-library-function
    "libsqlite3" "sqlite3_close"
    #:return-type int
    #:arg-types (list '*)))

(define foreign-sqlite3-prepare-v2
  (foreign-library-function
    "libsqlite3" "sqlite3_prepare_v2"
    #:return-type int
    #:arg-types (list '* '* int '* '*)))

(define foreign-sqlite3-step
  (foreign-library-function
    "libsqlite3" "sqlite3_step"
    #:return-type int
    #:arg-types (list '*)))

(define foreign-sqlite3-errmsg
  (foreign-library-function
    "libsqlite3" "sqlite3_errmsg"
    #:return-type '*
    #:arg-types (list '*)))

(define foreign-sqlite3-column-count
  (foreign-library-function
    "libsqlite3" "sqlite3_column_count"
    #:return-type int
    #:arg-types (list '*)))

(define foreign-sqlite3-column-text
  (foreign-library-function
    "libsqlite3" "sqlite3_column_text"
    #:return-type '*
    #:arg-types (list '* int)))

(define foreign-sqlite3-column-type
  (foreign-library-function
    "libsqlite3" "sqlite3_column_type"
    #:return-type int
    #:arg-types (list '* int)))

(define foreign-sqlite3-column-int
  (foreign-library-function
    "libsqlite3" "sqlite3_column_int"
    #:return-type int
    #:arg-types (list '* int)))

(define foreign-sqlite3-column-double
  (foreign-library-function
    "libsqlite3" "sqlite3_column_double"
    #:return-type double
    #:arg-types (list '* int)))

(define foreign-sqlite3-finalize
  (foreign-library-function
    "libsqlite3" "sqlite3_finalize"
    #:return-type int
    #:arg-types (list '*)))

(define foreign-sqlite3-bind-double
  (foreign-library-function
    "libsqlite3" "sqlite3_bind_double"
    #:return-type int
    #:arg-types (list '* int double)))

(define foreign-sqlite3-bind-int
  (foreign-library-function
    "libsqlite3" "sqlite3_bind_int"
    #:return-type int
    #:arg-types (list '* int int)))

(define foreign-sqlite3-bind-null
  (foreign-library-function
    "libsqlite3" "sqlite3_bind_null"
    #:return-type int
    #:arg-types (list '* int)))

(define foreign-sqlite3-bind-text
  (foreign-library-function
    "libsqlite3" "sqlite3_bind_text"
    #:return-type int
    #:arg-types (list '* int '* int '*)))

(define d
  (lambda args
    (for-each
      (lambda (arg) (display arg)(display " "))
      args)
    (newline)))

(define (sqlite3-open path)
  (define pp-db (bytevector->pointer (make-bytevector pointer-size)))
  (define ret
    (foreign-sqlite3-open
      (string->pointer path)
      pp-db))
  (when (not (= ret SQLITE_OK))
    (raise (list "Error in foreign-sqlite3-open" ret)))
  (dereference-pointer pp-db))

(define (sqlite3-close db)
  (define ret (foreign-sqlite3-close db))
  (when (not (= ret SQLITE_OK))
    (raise (list "Error in foreign-sqlite3-open" ret))))

(define (handle-error db code)
  (define msg (pointer->string (foreign-sqlite3-errmsg db)))
  (d msg)
  (raise-exception
    (string-append "Error: " msg)))

(define (sqlite3-col-val p-stmt i)
  ; No support for BLOB at the moment.
  (let ((col-type (foreign-sqlite3-column-type p-stmt i)))
    (cond
      ((= col-type SQLITE_INTEGER)
       (foreign-sqlite3-column-int p-stmt i))
      ((= col-type SQLITE_FLOAT)
       (foreign-sqlite3-column-double p-stmt i))
      ((= col-type SQLITE_TEXT)
       (pointer->string (foreign-sqlite3-column-text p-stmt i) -1 "utf-8"))
      ((= col-type SQLITE_NULL)
       '()))))

(define (sqlite3-row p-stmt)
  (let ((ncols (foreign-sqlite3-column-count p-stmt)))
    (let loop ((i 0) (row '()))
      (if (< i ncols)
        (loop (1+ i) (cons (sqlite3-col-val p-stmt i) row))
        (reverse row)))))

(define sqlite3-execute
  (case-lambda
    ((db sql) (sqlite3-execute db sql '()))
    ((db sql pars)
      (define pp-stmt (bytevector->pointer (make-bytevector pointer-size)))
      (let (
          (ret
            (foreign-sqlite3-prepare-v2
              db (string->pointer sql) -1 pp-stmt %null-pointer)))
        (cond
          ((= ret SQLITE_ERROR) (handle-error db ret))
          ((= ret SQLITE_OK) '())
          (else (raise (list "Error in foreign-sqlite3-prepare-v2" ret)))))
      (define p-stmt (dereference-pointer pp-stmt))
      (let loop ((pars pars) (pari 1))
        (when (not (null? pars))
          (let ((par (car pars)))
            (cond
              ((null? par)
               (foreign-sqlite3-bind-null p-stmt pari))
              ((integer? par)
               (foreign-sqlite3-bind-int p-stmt pari par))
              ((number? par)
               (foreign-sqlite3-bind-double p-stmt pari par))
              ((string? par)
               (let ((par-encoded (string->bytevector par "utf-8")))
                 (foreign-sqlite3-bind-text
                   p-stmt pari
                   (bytevector->pointer par-encoded)
                   (bytevector-length par-encoded) %null-pointer)))
              (else
                (raise (list "Unexpected parameter type")))))
          (loop (cdr pars) (1+ pari))))
      (let loop ((rows '()))
        (define ret (foreign-sqlite3-step p-stmt))
        (cond
          ((= ret SQLITE_DONE)
           (begin
             (foreign-sqlite3-finalize p-stmt)
             (reverse rows)))
          ((= ret SQLITE_ROW)
           (let ()
             (loop (cons (sqlite3-row p-stmt) rows))))
          ((= ret SQLITE_ERROR) (handle-error db ret))
          (else (raise (list "Error in foreign-sqlite3-step" ret))))))))

(define (display-strings pointer len)
  ; Display an array of pointers to strings.
  (let loop ((i 0))
    (when (< i len)
      (let (
          (address (+ (pointer-address pointer) (* i pointer-size))))
        (d (pointer->string (dereference-pointer (make-pointer address)))))
      (loop (1+ i)))))


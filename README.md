# guile-sqlite3

`guile-sqlite3` is a set of sqlite3 bindings for GNU Guile. The code is pure
Guile. The sqlite3 library is loaded at runtime using Guile's foreign function
interface.

## Example use

Clone the repo

```
git clone git@github.com:gtzampanakis/guile-sqlite3.git
```

Place the following into file main.scm:

```scheme
(use-modules (db sqlite3))

(define db-path ":memory:")
(define db (sqlite3-open db-path))

(sqlite3-execute-sql db
    "create table tbl (x integer)")

(let loop ((i 0))
    (when (< i 10)
        (sqlite3-execute-sql db
            "insert into tbl (x) values (?)"
            (list i))
        (loop (1+ i))))

(for-each
    (lambda (row)
        (display row)
        (newline))
    (sqlite3-execute-sql db
        "select x from tbl where ?=? order by x"
        (list 1 1)))
```

Finally, run with

```bash
guile -L guile-sqlite3/modules main.scm
```

For all available procedures please examine the source code. Their usage is
quite obvious.

# sql-migrate

Alembic-style **revision runner** for [cl-stack](https://github.com/egao1980/cl-stack). Versions [`sql-orm`](https://github.com/egao1980/sql-orm) `schema-op` / `schema-migration` — it does not invent a second DDL algebra.

Linear history in 0.1.0 (no branches). Scripts live in a `script-directory` (in-memory). File layout later.

```lisp
(asdf:load-system "sql-migrate")

(let ((dir (stack-sql-migrate:make-script-directory)))
  (stack-sql-migrate:make-revision dir nil snap-v1 :id "0001" :name "widgets")
  (stack-sql-migrate:make-revision dir snap-v1 snap-v2 :id "0002" :name "add-color")
  (sql-orm:with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
    (stack-sql-migrate:upgrade c dir)              ; → head
    (stack-sql-migrate:current-revision c dir)     ; "0002"
    (stack-sql-migrate:downgrade c dir :count 1))) ; → "0001"
```

Version table: `sql_migrate_version` (one row, Alembic-shaped).

## License

MIT — see [LICENSE](LICENSE).

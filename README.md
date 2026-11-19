# sql-migrate

Alembic-style **revision runner** for [cl-stack](https://github.com/egao1980/cl-stack). Versions [`sql-orm`](https://github.com/egao1980/sql-orm) `schema-op` / `schema-migration` — it does not invent a second DDL algebra.

Version **0.2.1** (S2): the `script-directory` is a [`revision-protocol`](https://github.com/egao1980/revision-protocol) store (`:primary :delta`), a `schema-diff-engine` diffs [`sql-orm`](https://github.com/egao1980/sql-orm) snapshots (with `render-diff :sql`), and `upgrade` / `downgrade` run as `plan-transition` + `execute-plan` on an `sql-target`, each step's DDL and stamp in one transaction. The 0.1 API is unchanged on linear graphs; forks are refused at planning (branches and merges are S3). A failing step signals one condition that is both a `revision-protocol:step-failed` (with `retry-step` / `skip-step` / `rollback-to-start`) and the driver's own condition class. Scripts are in memory; a file layout comes later.

**0.2.0 (S2, contract on this branch):** the `script-directory` becomes a [`revision-protocol`](https://github.com/egao1980/revision-protocol) store (`:primary :delta`, content = each migration's schema ops), diffs come from a [`diff-protocol`](https://github.com/egao1980/diff-protocol) `schema-diff-engine` (footprints, lossy drops, `render-diff :sql`), and a `sql-target` runs `plan-transition` / `execute-plan` with each step's DDL and stamp in one transaction. The 0.1 API stays. Brief: [cl-stack `docs/capabilities/sql.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/sql.md) → "sql-migrate 0.2.0 — S2 locked".

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

## Step failures

A failing step signals one condition of a class made for the driver's condition class: it inherits `revision-protocol:step-failed` and the driver's class (`STEP-FAILED/SQLITE-CONSTRAINT-ERROR`), so handlers on either fire.

- `step-failed-cause` is the driver's original condition. Its bound slots that have an initarg are also copied into the combined condition (closer-mop, every implementation), so the driver's readers work on it.
- A driver slot whose initarg is `:step`, `:cause` or `:attempt` gets `step-failed`'s value, not the driver's.
- `princ` prints the `step-failed` report, which prints the cause once. The driver's own `print-object` methods (cl-sqlite's `Code` / `Database` / `SQL` block) run for the cause only, not again for the combined condition.
- One class per driver class, built once under a lock.

## License

MIT — see [LICENSE](LICENSE).

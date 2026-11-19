(defsystem "sql-migrate"
  :version "0.2.0"
  :description "Alembic-style revision runner on sql-orm schema-op (stack-sql-migrate): a revision-protocol store of schema-op migrations, a diff-protocol schema engine, and a SQL revision target"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "sql-orm" "0.2.0")
               (:version "revision-protocol" "0.2.0")
               (:version "diff-protocol" "0.2.0"))
  :properties
  (:cl-repo
   (:ci (:with ("sql-backend-sqlite3" "sql-query-sqlite3" "sql-query-postgres")
         :load-before-test ("sql-backend-sqlite3" "sql-query-sqlite3"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "engine")
               (:file "directory")
               (:file "target")
               (:file "runner"))
  :in-order-to ((test-op (test-op "sql-migrate/tests"))))

(defsystem "sql-migrate/tests"
  :depends-on ("sql-migrate"
               "sql-backend-sqlite3"
               "sql-query-sqlite3"
               "sql-query-postgres"
               "revision-protocol"
               "diff-protocol"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "runner-test")
               (:file "s2-fixtures")
               (:file "engine-test")
               (:file "directory-test")
               (:file "target-test")
               (:file "compat-test")
               (:file "condition-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))

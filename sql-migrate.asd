(defsystem "sql-migrate"
  :version "0.1.0"
  :description "Alembic-style revision runner on sql-orm schema-op (stack-sql-migrate)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("sql-orm")
  :properties
  (:cl-repo
   (:ci (:with ("sql-backend-sqlite3" "sql-query-sqlite3")
         :load-before-test ("sql-backend-sqlite3" "sql-query-sqlite3"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "directory")
               (:file "runner"))
  :in-order-to ((test-op (test-op "sql-migrate/tests"))))

(defsystem "sql-migrate/tests"
  :depends-on ("sql-migrate"
               "sql-backend-sqlite3"
               "sql-query-sqlite3"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "runner-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))

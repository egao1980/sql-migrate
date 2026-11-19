(in-package #:sql-migrate/tests)

;;; S2 — SQL-TARGET on SQLite in memory: stamps, reflected state, drift,
;;; transactions, EXECUTE-PLAN (laws 5, transactional steps, restarts,
;;; resume). Every HANDLER-BIND sits inside its OK form (Rove's OK binds its
;;; own ERROR handler), and restarts are recorded inside the handler.

(defun %target (c dir &rest keys)
  (apply #'make-sql-target c :directory dir keys))

(defun %reject-stamp-of (c dir revision)
  "Make the version table refuse REVISION (the step's DDL runs, its stamp fails)."
  (%reject-stamp c dir revision))

(defun %drop-reject-stamp (c)
  (sql-protocol:execute c "DROP TRIGGER reject_stamp"))

(deftest sql-target-defaults
  (let ((dir (make-script-directory :version-table "my_versions")))
    (with-sqlite (c)
      (let ((target (%target c dir)))
        (ok (typep target 'rev:revision-target))
        (ok (eq c (target-connection target)))
        (ok (eq dir (target-directory target)))
        (ok (equal "my_versions" (target-version-table target)))
        (ok (eq :sqlite3 (schema-dialect-key (target-dialect target))))
        (ok (target-check-drift-p target))
        (ok (rev:target-transactional-p target))
        (ok (null (rev:target-fidelities target)) "projections are S4"))
      (ok (equal "sql_migrate_version" (target-version-table (make-sql-target c))))
      (ok (not (target-check-drift-p (make-sql-target c :check-drift nil))))
      (ok (not (rev:target-transactional-p (make-sql-target c :dialect :ansi)))))))

(deftest sql-target-stamps
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (let ((target (%target c dir)))
        (ok (null (rev:target-revisions target)))
        (ok (table-exists-p c "sql_migrate_version") "read creates the table, as 0.1")
        (ok (equal '("0001") (rev:target-stamp target '("0001"))))
        (ok (equal '("0001") (rev:target-revisions target)))
        (ok (equal "0001" (current-revision c dir)))
        (ok (equal '("0002" "0001")
                   (rev:target-stamp target (list (rev:resolve-revision dir "0002") "0001" "0002")))
            "revisions or ids, in order, deduped")
        (ok (equal '("0001" "0002") (rev:target-revisions target)) "read back sorted")
        (ok (equal '("0001" "0002") (version-rows c dir)))
        (ok (equal '("zzzz") (rev:target-stamp target '("zzzz"))) "targets know no store")
        (ok (null (rev:target-stamp target nil)))
        (ok (null (version-rows c dir)))))))

(deftest sql-target-state-is-reflected
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (let ((target (%target c dir)))
        (ok (null (rev:target-state target)) "the version table is excluded")
        (upgrade c dir)
        (let ((state (rev:target-state target)))
          (ok (equal '("widgets") (mapcar #'car state)) "reflected names are strings")
          (ok (schema-snapshot-equal (%snap-v2) state :dialect :sqlite3)))
        (ok (signals (setf (rev:target-state target) nil) 'rev:revision-error))))))

(deftest sql-target-drift
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (let ((target (%target c dir)))
        (ok (null (rev:target-drift target dir)) "fresh database, unstamped")
        (upgrade c dir)
        (ok (null (rev:target-drift target dir))
            "compared under the target's dialect: TEXT = :text on SQLite")
        (sql-protocol:execute c "ALTER TABLE widgets ADD COLUMN extra TEXT")
        (let ((drift (rev:target-drift target dir)))
          (ok (typep drift 'schema-diff))
          (ok (equal '(:add) (change-kinds drift)))
          (ok (equal '("column:widgets.extra") (diff:diff-footprint drift))))
        (rev:target-stamp target '("0001" "0002"))
        (ok (signals (rev:target-drift target dir) 'rev:revision-error))))))

(deftest sql-target-drift-unstamped
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (let ((target (%target c dir)))
        (sql-protocol:execute c "CREATE TABLE stray (x INTEGER)")
        (let ((drift (rev:target-drift target dir)))
          (ok (equal '(:add) (change-kinds drift)))
          (ok (equal '("table:stray") (diff:diff-footprint drift))))))))

;;; ---------------------------------------------------------------------------
;;; Transactions
;;; ---------------------------------------------------------------------------

(deftest sql-target-transaction-rollback
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (let ((target (%target c dir)))
        (rev:target-stamp target '("0001"))
        (ok (eq :done (rev:with-target-transaction (target) :done)))
        (ok (signals (rev:with-target-transaction (target)
                       (sql-protocol:execute c "CREATE TABLE scratch (x INTEGER)")
                       (rev:target-stamp target '("0002"))
                       (error "boom"))
                     'simple-error))
        (ok (not (table-exists-p c "scratch")) "DDL rolled back")
        (ok (equal '("0001") (rev:target-revisions target)) "stamp restored")))))

(deftest sql-target-transaction-refused
  (let ((ran nil))
    (with-sqlite (c)
      (let ((target (make-sql-target c :dialect :ansi)))
        (ok (signals (rev:with-target-transaction (target) (setf ran t))
                     'rev:revision-error))
        (ok (not ran) "body not run")))))

;;; ---------------------------------------------------------------------------
;;; execute-plan
;;; ---------------------------------------------------------------------------

(deftest sql-target-execute-up-and-down
  "Law 5: the goal is reached and stamped; each step stamps its TO."
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (let ((target (%target c dir)))
        (multiple-value-bind (revs skipped)
            (rev:execute-plan target (rev:plan-transition dir target "0002"))
          (ok (equal '("0002") revs))
          (ok (null skipped)))
        (ok (equal '("0002") (version-rows c dir)))
        (ok (%has-column-p c "widgets" "color"))
        (ok (null (rev:target-drift target dir)))
        (multiple-value-bind (revs skipped)
            (rev:execute-plan target (rev:plan-transition dir target nil))
          (ok (null revs))
          (ok (null skipped)))
        (ok (not (table-exists-p c "widgets")))
        (ok (null (version-rows c dir)))))))

(deftest sql-target-step-ddl-and-stamp-atomic
  "A step whose stamp fails leaves neither its DDL nor its stamp; the earlier
step stays committed (the 0.1.1 guarantee)."
  (let ((dir (linear-dir))
        (seen nil))
    (with-sqlite (c)
      (let ((target (%target c dir)))
        (%reject-stamp-of c dir "0002")
        (ok (signals (rev:execute-plan target (rev:plan-transition dir target "0002"))
                     'rev:step-failed))
        (ok (equal '("0001") (rev:target-revisions target)))
        (ok (%has-column-p c "widgets" "name"))
        (ok (not (%has-column-p c "widgets" "color")))
        ;; Again from 0001, recording the condition and its restarts inside
        ;; the handler, then skipping the step.
        (ok (equal '("0001")
                   (handler-bind ((rev:step-failed
                                    (lambda (s)
                                      (setf seen (list (rev:step-to (rev:step-failed-step s))
                                                       (rev:step-failed-attempt s)
                                                       (typep (rev:step-failed-cause s) 'rev:revision-error)
                                                       (typep (rev:step-failed-cause s) 'error)
                                                       (restart-names s)
                                                       (%has-column-p c "widgets" "color")))
                                      (invoke-restart 'rev:skip-step))))
                     (rev:execute-plan target (rev:plan-transition dir target "0002")))))
        (ok (not (sixth seen)) "signalled after the step's transaction rolled back")
        (ok (equal "0002" (first seen)))
        (ok (eql 1 (second seen)))
        (ok (not (third seen)) "the cause is the driver's error")
        (ok (fourth seen))
        (ok (member 'rev:retry-step (fifth seen)))
        (ok (member 'rev:skip-step (fifth seen)))
        (ok (member 'rev:rollback-to-start (fifth seen)))))))

(deftest sql-target-step-failed-retry
  (let ((dir (linear-dir))
        (attempts '()))
    (with-sqlite (c)
      (let ((target (%target c dir)))
        (%reject-stamp-of c dir "0002")
        (ok (equal '("0002")
                   (handler-bind ((rev:step-failed
                                    (lambda (s)
                                      (push (rev:step-failed-attempt s) attempts)
                                      (%drop-reject-stamp c)
                                      (invoke-restart 'rev:retry-step))))
                     (rev:execute-plan target (rev:plan-transition dir target "0002")))))
        (ok (equal '(1) attempts))
        (ok (%has-column-p c "widgets" "color"))
        (ok (equal '("0002") (version-rows c dir)))))))

(deftest sql-target-step-failed-skip
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (let ((target (%target c dir)))
        (%reject-stamp-of c dir "0002")
        (multiple-value-bind (revs skipped)
            (handler-bind ((rev:step-failed
                             (lambda (s)
                               (declare (ignore s))
                               (invoke-restart 'rev:skip-step))))
              (rev:execute-plan target (rev:plan-transition dir target "0002")))
          (ok (equal '("0001") revs) "a skipped step is not stamped")
          (ok (equal '("0002") (mapcar #'rev:step-to skipped))))
        (ok (not (%has-column-p c "widgets" "color")))))))

(deftest sql-target-step-failed-rollback-to-start
  "ROLLBACK-TO-START undoes the steps this call completed and restamps the start."
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (let ((target (%target c dir)))
        (%reject-stamp-of c dir "0002")
        (ok (null (handler-bind ((rev:step-failed
                                   (lambda (s)
                                     (declare (ignore s))
                                     (invoke-restart 'rev:rollback-to-start))))
                    (rev:execute-plan target (rev:plan-transition dir target "0002")))))
        (ok (null (version-rows c dir)))
        (ok (not (table-exists-p c "widgets")) "step 0001's DDL undone")))))

(deftest sql-target-drift-blocks-execute
  (let ((dir (linear-dir))
        (restarts nil))
    (with-sqlite (c)
      (let ((target (%target c dir)))
        (upgrade c dir :count 1)
        (sql-protocol:execute c "CREATE TABLE stray (x INTEGER)")
        (ok (signals (rev:execute-plan target (rev:plan-transition dir target "0002"))
                     'rev:target-drift-detected))
        (ok (null (handler-bind ((rev:target-drift-detected
                                   (lambda (d)
                                     (setf restarts (restart-names d))
                                     (invoke-restart 'abort))))
                    (rev:execute-plan target (rev:plan-transition dir target "0002")))))
        (ok (member 'abort restarts))
        (ok (member 'rev:discard-drift restarts))
        (ok (not (member 'rev:capture-and-continue restarts)) "capture is S4")
        (ok (equal '("0001") (version-rows c dir)) "nothing changed")
        (ok (not (%has-column-p c "widgets" "color")))
        (ok (table-exists-p c "stray"))
        ;; With :check-drift nil (what UPGRADE uses) the plan just runs.
        (ok (equal '("0002")
                   (rev:execute-plan (%target c dir :check-drift nil)
                                     (rev:plan-transition dir target "0002"))))
        (ok (table-exists-p c "stray"))))))

(deftest sql-target-discard-drift
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (let ((target (%target c dir)))
        (upgrade c dir :count 1)
        (sql-protocol:execute c "ALTER TABLE widgets ADD COLUMN extra TEXT")
        (ok (equal '("0002")
                   (handler-bind ((rev:target-drift-detected
                                    (lambda (d)
                                      (declare (ignore d))
                                      (invoke-restart 'rev:discard-drift))))
                     (rev:execute-plan target (rev:plan-transition dir target "0002")))))
        (ok (not (%has-column-p c "widgets" "extra")) "drift reverted")
        (ok (%has-column-p c "widgets" "color"))
        (ok (null (rev:target-drift target dir)))))))

(deftest sql-target-resume
  "A plan resumes after the step whose TO is the current stamp; a foreign stamp is refused."
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (let* ((target (%target c dir))
             (plan (rev:plan-transition dir target "0002")))
        (upgrade c dir :count 1)
        (ok (equal '("0002") (rev:execute-plan target plan)))
        (ok (%has-column-p c "widgets" "color"))
        (rev:target-stamp target '("zzzz"))
        (ok (signals (rev:execute-plan (%target c dir :check-drift nil) plan)
                     'rev:revision-error))))))

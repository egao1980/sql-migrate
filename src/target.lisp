(in-package #:sql-migrate)

;;; SQL-TARGET (S2): a revision-protocol target over a database connection
;;; and a version table. Brief: "sql-migrate 0.2.0 — S2 locked" → "SQL target".
;;;
;;;   TARGET-REVISIONS          version_num rows, sorted STRING< (creates the
;;;                             table when missing, as 0.1 CURRENT-REVISION)
;;;   TARGET-STAMP              DELETE all rows, INSERT each id; no store check,
;;;                             no transaction of its own
;;;   TARGET-STATE              SQL-ORM:REFLECT-SCHEMA, version table excluded
;;;   (SETF TARGET-STATE)       STACK-REV:REVISION-ERROR in S2
;;;   TARGET-DRIFT              stamped REVISION-STATE vs TARGET-STATE under the
;;;                             target's dialect → NIL or a SCHEMA-DIFF
;;;   TARGET-FIDELITIES         NIL in S2 (projections are S4)
;;;   TARGET-TRANSACTIONAL-P    dialect key :SQLITE3 or :POSTGRES
;;;   CALL-WITH-TARGET-TRANSACTION  SQL-PROTOCOL:WITH-TRANSACTION
;;;   EXECUTE-PLAN              R0b contract; each step's DDL and stamp in one
;;;                             transaction

(defclass sql-target (rev:revision-target)
  ((connection :initarg :connection :reader target-connection
               :documentation "An sql-orm / sql-protocol connection.")
   (directory :initarg :directory :initform nil :reader target-directory
              :documentation "The SCRIPT-DIRECTORY the stamps come from, or NIL.")
   (version-table :initarg :version-table :reader target-version-table)
   (dialect :initarg :dialect :initform nil :reader target-dialect
            :documentation "Dialect designator; NIL → the connection's.")
   (check-drift :initarg :check-drift :initform t :reader target-check-drift-p
                :documentation "When true EXECUTE-PLAN checks TARGET-DRIFT before
its first step (R0b). UPGRADE / DOWNGRADE use NIL (0.1 never checked)."))
  (:documentation "A database as a STACK-REV revision target: schema =
REFLECT-SCHEMA of the connection, stamp = the version table's rows."))

(defun make-sql-target (connection &key directory version-table dialect
                                     (check-drift t))
  "A SQL-TARGET over CONNECTION. VERSION-TABLE defaults to DIRECTORY's
(DIRECTORY-VERSION-TABLE), else \"sql_migrate_version\". DIALECT defaults to
(SQL-QUERY:DIALECT-FOR-CONNECTION connection), resolved when the target is
made: TARGET-DIALECT never returns NIL."
  (declare (ignore connection directory version-table dialect check-drift))
  (%not-implemented 'make-sql-target))

(defmethod rev:target-revisions ((target sql-target))
  (%not-implemented 'target-revisions))

(defmethod rev:target-stamp ((target sql-target) revisions)
  (declare (ignore revisions))
  (%not-implemented 'target-stamp))

(defmethod rev:target-state ((target sql-target))
  (%not-implemented 'target-state))

(defmethod (setf rev:target-state) (state (target sql-target))
  (declare (ignore state))
  (%not-implemented '(setf target-state)))

(defmethod rev:target-drift ((target sql-target) store)
  (declare (ignore store))
  (%not-implemented 'target-drift))

(defmethod rev:target-fidelities ((target sql-target))
  (%not-implemented 'target-fidelities))

(defmethod rev:target-transactional-p ((target sql-target))
  (%not-implemented 'target-transactional-p))

(defmethod rev:call-with-target-transaction ((target sql-target) thunk)
  (declare (ignore thunk))
  (%not-implemented 'call-with-target-transaction))

(defmethod rev:execute-plan ((target sql-target) plan)
  "R0b EXECUTE-PLAN on a database. Per step, when TARGET-TRANSACTIONAL-P, one
transaction runs the step's DDL (SCHEMA-DIFF-OPS of STEP-DIFF through
SQL-ORM:UPGRADE-SCHEMA :TRANSACTION NIL) and stamps (STEP-TO); otherwise DDL
then stamp. Any error inside that transaction rolls it back and signals
STEP-FAILED (CAUSE = the error) with RETRY-STEP, SKIP-STEP and
ROLLBACK-TO-START (runs the inverses of the steps this call completed and
restamps PLAN-START, in one transaction; returns NIL). With
TARGET-CHECK-DRIFT-P, drift before the first step signals
TARGET-DRIFT-DETECTED with DISCARD-DRIFT (run the complete diff from the
actual schema back to the stamped one, then the plan) and ABORT (return NIL);
CAPTURE-AND-CONTINUE is not offered in S2. Resume and IRREVERSIBLE-STEP as
R0b."
  (declare (ignore plan))
  (%not-implemented 'execute-plan))

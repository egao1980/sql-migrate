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

(defun %conn (connection)
  (sql-orm:orm-connection connection))

(defun %row-get (row key)
  (getf row (intern (string-upcase (string key)) :keyword)))

(defun %ensure-table (connection table)
  "CREATE TABLE IF NOT EXISTS the version table TABLE on CONNECTION → TABLE."
  (sql-protocol:execute
   (%conn connection)
   (format nil "CREATE TABLE IF NOT EXISTS ~A (version_num TEXT NOT NULL PRIMARY KEY)"
           table))
  table)

(defun make-sql-target (connection &key directory version-table dialect
                                     (check-drift t))
  "A SQL-TARGET over CONNECTION. VERSION-TABLE defaults to DIRECTORY's
(DIRECTORY-VERSION-TABLE), else \"sql_migrate_version\". DIALECT defaults to
(SQL-QUERY:DIALECT-FOR-CONNECTION connection), resolved when the target is
made: TARGET-DIALECT never returns NIL."
  (check-type directory (or null script-directory))
  (make-instance 'sql-target
                 :connection connection
                 :directory directory
                 :version-table (or version-table
                                    (and directory (directory-version-table directory))
                                    "sql_migrate_version")
                 :dialect (or dialect
                              (sql-query:dialect-for-connection (%conn connection)))
                 :check-drift (and check-drift t)))

(defun %stamp-id (x)
  (etypecase x
    (rev:revision (rev:revision-id x))
    (string x)))

(defmethod rev:target-revisions ((target sql-target))
  (let* ((table (%ensure-table (target-connection target) (target-version-table target)))
         (rows (sql-protocol:fetch-all
                (sql-protocol:execute (%conn (target-connection target))
                                      (format nil "SELECT version_num FROM ~A" table)))))
    (sort (mapcar (lambda (row) (%row-get row :version_num)) rows) #'string<)))

(defmethod rev:target-stamp ((target sql-target) revisions)
  (check-type revisions list)
  (let ((conn (%conn (target-connection target)))
        (table (%ensure-table (target-connection target) (target-version-table target)))
        (ids (remove-duplicates (mapcar #'%stamp-id revisions)
                                :test #'equal :from-end t)))
    (sql-protocol:execute conn (format nil "DELETE FROM ~A" table))
    (dolist (id ids)
      (sql-protocol:execute conn
                            (format nil "INSERT INTO ~A (version_num) VALUES (?)" table)
                            (list id)))
    (copy-list ids)))

(defmethod rev:target-state ((target sql-target))
  (sql-orm:reflect-schema (%conn (target-connection target))
                          :exclude (list (target-version-table target))
                          :dialect (target-dialect target)))

(defmethod (setf rev:target-state) (state (target sql-target))
  (declare (ignore state))
  (error 'rev:revision-error
         :message "a SQL target's schema cannot be set outside a plan (S2)"))

(defun %drift-engine (target)
  (make-schema-diff-engine :dialect (target-dialect target)))

(defun %stamped-state (target store)
  "State of TARGET's stamped revision in STORE (NIL unstamped);
REVISION-ERROR for several stamps."
  (let ((ids (rev:target-revisions target)))
    (when (rest ids)
      (error 'rev:revision-error
             :message (format nil "target is stamped with several revisions ~S" ids)))
    (and ids (rev:revision-state store (first ids)))))

(defmethod rev:target-drift ((target sql-target) store)
  (let ((engine (%drift-engine target))
        (stamped (%stamped-state target store))
        (actual (rev:target-state target)))
    (unless (diff:state-equal engine stamped actual)
      (diff:compute-diff engine stamped actual :level :changes))))

(defmethod rev:target-fidelities ((target sql-target))
  nil)

(defmethod rev:target-transactional-p ((target sql-target))
  (and (member (sql-orm:schema-dialect-key (target-dialect target)) '(:sqlite3 :postgres))
       t))

(defmethod rev:call-with-target-transaction ((target sql-target) thunk)
  (unless (rev:target-transactional-p target)
    (error 'rev:revision-error
           :message (format nil "dialect ~S has no transactional DDL"
                            (sql-orm:schema-dialect-key (target-dialect target)))))
  (sql-protocol:with-transaction ((%conn (target-connection target)))
    (funcall thunk)))

;;; ------------------------------------------------------------------
;;; execute-plan
;;; ------------------------------------------------------------------

(defun %unit (target ops stamp-ids)
  "Run OPS, then stamp STAMP-IDS: in one transaction when TARGET is
transactional, otherwise in order."
  (flet ((run ()
           (sql-orm:upgrade-schema (target-connection target) ops :transaction nil)
           (rev:target-stamp target stamp-ids)))
    (if (rev:target-transactional-p target)
        (rev:call-with-target-transaction target #'run)
        (run))))

(defun %try-unit (target ops stamp-ids)
  "→ NIL, or the error that stopped the unit. HANDLER-CASE unwinds through
the transaction first, so it has already rolled back."
  (handler-case (progn (%unit target ops stamp-ids) nil)
    (error (cause) cause)))

;;; One combined condition class per driver condition class: it inherits
;;; STACK-REV:STEP-FAILED and the driver's class, so a handler on either sees
;;; the failure, and a catch-all ERROR handler sees one condition.

(defvar *step-failed-classes* (make-hash-table :test 'eq)
  "Driver condition class → the combined STEP-FAILED class made for it.")

(defun %step-failed-class (cause)
  "Name of the condition class signalled for a step failing with CAUSE."
  (let* ((class (class-of cause))
         (name (class-name class)))
    (if (or (null name)
            (not (eq class (find-class name nil)))
            (subtypep class 'rev:step-failed))
        'rev:step-failed
        (or (gethash class *step-failed-classes*)
            (let ((combined (make-symbol (format nil "STEP-FAILED/~A" name))))
              ;; The driver class is only known at run time.
              (eval `(define-condition ,combined (rev:step-failed ,name) ()
                       (:documentation "A plan step failed with a driver condition of this class.")))
              (setf (gethash class *step-failed-classes*) combined))))))

(defun %cause-initargs (cause)
  "Initargs that copy CAUSE's bound slots into a condition of a subclass."
  #+sbcl
  (loop for slot in (sb-mop:class-slots (class-of cause))
        for name = (sb-mop:slot-definition-name slot)
        for initarg = (first (sb-mop:slot-definition-initargs slot))
        when (and initarg (slot-boundp cause name))
          append (list initarg (slot-value cause name)))
  #-sbcl
  (progn cause nil))

(defun %signal-step-failed (step cause attempt)
  (let ((class (%step-failed-class cause)))
    (apply #'error class
           :step step :cause cause :attempt attempt
           (if (eq class 'rev:step-failed) '() (%cause-initargs cause)))))

(defun %remaining-steps (plan stamp)
  "Steps of PLAN still to run on a target stamped STAMP: all of them at
PLAN-START, the steps after the one whose TO is STAMP otherwise.
REVISION-ERROR for a stamp that is neither."
  (let ((steps (rev:plan-steps plan)))
    (if (equal stamp (rev:plan-start plan))
        steps
        (loop for tail on steps
              when (equal stamp (let ((to (rev:step-to (first tail))))
                                  (and to (list to))))
                return (rest tail)
              finally (error 'rev:revision-error
                             :message (format nil "target stamp ~S is neither the plan's start nor any step's TO"
                                              stamp))))))

(defun %step-stamp (step)
  (let ((to (rev:step-to step)))
    (and to (list to))))

(defmethod rev:execute-plan ((target sql-target) plan)
  "R0b EXECUTE-PLAN on a database. Per step, when TARGET-TRANSACTIONAL-P, one
transaction runs the step's DDL (SCHEMA-DIFF-OPS of STEP-DIFF through
SQL-ORM:UPGRADE-SCHEMA :TRANSACTION NIL) and stamps (STEP-TO); otherwise DDL
then stamp. Any error inside that transaction rolls it back; then one
condition is signalled that is both a STEP-FAILED (CAUSE = the error) and of
the error's class, with RETRY-STEP, SKIP-STEP and ROLLBACK-TO-START (runs the
inverses of the steps this call completed and restamps PLAN-START, in one
transaction; returns NIL). With TARGET-CHECK-DRIFT-P, drift before the first
step signals TARGET-DRIFT-DETECTED with DISCARD-DRIFT (run the complete diff
from the actual schema back to the stamped one, then the plan) and ABORT
(return NIL); CAPTURE-AND-CONTINUE is not offered in S2. Resume and
IRREVERSIBLE-STEP as R0b. Completion returns two values; ABORT and
ROLLBACK-TO-START return the single value NIL."
  (let* ((store (rev:plan-store plan))
         (stamp (rev:target-revisions target))
         (steps (%remaining-steps plan stamp))
         (done '())
         (skipped '()))
    (when (target-check-drift-p target)
      (when (rev:target-drift target store)
        (restart-case (error 'rev:target-drift-detected
                             :target target
                             :drift (rev:target-drift target store))
          (rev:discard-drift ()
            :report "Revert the schema to the stamped revision's, then run the plan."
            (let ((engine (%drift-engine target)))
              (%unit target
                     (schema-diff-ops (diff:compute-diff engine
                                                         (rev:target-state target)
                                                         (%stamped-state target store)))
                     stamp)))
          (abort ()
            :report "Run nothing; return NIL."
            (return-from rev:execute-plan nil)))))
    (dolist (step steps (values (rev:target-revisions target) (nreverse skipped)))
      (block one-step
        (unless (rev:step-reversible-p step)
          (restart-case (error 'rev:irreversible-step :step step)
            (rev:skip-step ()
              :report "Leave the schema and the stamp; continue with the next step."
              (push step skipped)
              (return-from one-step))
            (rev:stamp-only ()
              :report "Stamp the step's TO without changing the schema; continue."
              (rev:target-stamp target (%step-stamp step))
              (return-from one-step))
            (rev:abort-transition ()
              :report "Stop here; return NIL."
              (return-from rev:execute-plan nil))))
        (loop for attempt from 1
              for cause = (%try-unit target (schema-diff-ops (rev:step-diff step))
                                     (%step-stamp step))
              while cause
              do (restart-case (%signal-step-failed step cause attempt)
                   (rev:retry-step ()
                     :report "Run the step again."
                     nil)
                   (rev:skip-step ()
                     :report "Leave the schema and the stamp; continue with the next step."
                     (push step skipped)
                     (return-from one-step))
                   (rev:rollback-to-start ()
                     :report "Undo the steps this call completed, restamp the plan's start; return NIL."
                     (%unit target
                            (loop for s in done ; newest first
                                  append (schema-diff-ops (diff:invert-diff (rev:step-diff s))))
                            (rev:plan-start plan))
                     (return-from rev:execute-plan nil))))
        (push step done)))))

(in-package #:sql-migrate)

;;; SCRIPT-DIRECTORY (S2): a revision-protocol in-memory store with
;;; :PRIMARY :DELTA and a SCHEMA-DIFF-ENGINE. Each registered migration is
;;; one revision: id = :REVISION, parents = (:DOWN-REVISION) or none,
;;; content = (MAKE-SCHEMA-DIFF (MIGRATION-UPGRADE-OPS m)), so
;;; STACK-REV:REVISION-STATE folds the ops from the root with
;;; SQL-ORM:APPLY-OPS-TO-SNAPSHOT.
;;;
;;; CONTRACT: the 0.1 functions below still use their own id table
;;; (BY-ID) so the 0.1 suite keeps passing; the S2 implementation moves them
;;; onto the store (COMMIT-REVISION, HEADS, WALK-REVISIONS).

(defclass script-directory (rev:in-memory-store)
  ((version-table :initarg :version-table :initform "sql_migrate_version"
                  :reader directory-version-table)
   (by-id :initform (make-hash-table :test #'equal) :reader directory-by-id))
  (:documentation "Alembic-style script directory: a STACK-REV store of
schema-op migrations (CONTENT-PRIMARY :DELTA, CONTENT-ENGINE a
SCHEMA-DIFF-ENGINE) plus the name of its version table."))

(defun make-script-directory (&key (version-table "sql_migrate_version") dialect)
  "A fresh, empty SCRIPT-DIRECTORY. VERSION-TABLE names the table that
stamps a database. DIALECT is the CONTENT-ENGINE's dialect (NIL = ANSI
equality), used when the store diffs its own states."
  (make-instance 'script-directory
                 :version-table version-table
                 :primary :delta
                 :engine (make-schema-diff-engine :dialect dialect)))

(defun register-revision (dir migration)
  "Add a SCHEMA-MIGRATION → MIGRATION. REVISION must be a non-empty string,
unique in DIR (else MIGRATE-ERROR). S2: DOWN-REVISION must be NIL or a
registered id (else UNKNOWN-REVISION); the migration is committed to DIR as a
revision whose content is a SCHEMA-DIFF of its upgrade ops. Forks are
accepted (S3 plans them)."
  (check-type dir script-directory)
  (check-type migration sql-orm:schema-migration)
  (let ((id (sql-orm:schema-migration-revision migration)))
    (unless (and (stringp id) (plusp (length id)))
      (error 'migrate-error :message "migration needs a non-empty :revision"))
    (when (gethash id (directory-by-id dir))
      (error 'migrate-error
             :revision id
             :message (format nil "duplicate revision ~S" id)))
    (setf (gethash id (directory-by-id dir)) migration)
    migration))

(defun find-revision (dir id)
  "The SCHEMA-MIGRATION registered under exactly ID, or NIL. No id-prefix
matching (unlike STACK-REV:RESOLVE-REVISION on DIR)."
  (check-type dir script-directory)
  (gethash id (directory-by-id dir)))

(defun require-revision (dir id)
  "FIND-REVISION or signal UNKNOWN-REVISION (restart USE-VALUE: another id)."
  (or (find-revision dir id)
      (restart-case
          (error 'unknown-revision
                 :revision id
                 :message (format nil "unknown revision ~S" id))
        (use-value (replacement)
          :report "Use another revision id"
          (require-revision dir replacement)))))

(defun %child-map (dir)
  (let ((kids (make-hash-table :test #'equal)))
    (maphash (lambda (id mig)
               (declare (ignore id))
               (let ((down (sql-orm:schema-migration-down-revision mig)))
                 (push mig (gethash down kids))))
             (directory-by-id dir))
    kids))

(defun history (dir)
  "SCHEMA-MIGRATION list, parents before children (root to head on a linear
graph). S2: the order of STACK-REV:WALK-REVISIONS :TOPO from every head; a
forked directory is listed too (no BRANCH-NOT-SUPPORTED)."
  (check-type dir script-directory)
  (when (zerop (hash-table-count (directory-by-id dir)))
    (return-from history nil))
  (let ((kids (%child-map dir)))
    (maphash (lambda (down children)
               (when (> (length children) 1)
                 (error 'branch-not-supported
                        :revision down
                        :message (format nil "branched after ~S (0.1.0 is linear)" down))))
             kids)
    (let ((chain '())
          (cur (first (gethash nil kids))))
      (unless cur
        (error 'migrate-error :message "no root revision (down-revision NIL)"))
      (loop
        (push cur chain)
        (let ((next (gethash (sql-orm:schema-migration-revision cur) kids)))
          (unless next
            (return (nreverse chain)))
          (setf cur (first next)))))))

(defun head-revision (dir)
  "Revision id of the unique head, or NIL if empty. S2: several heads →
MULTIPLE-HEADS (heads sorted STRING<) with restart USE-VALUE (an id, returned
as is)."
  (let ((chain (history dir)))
    (when chain
      (sql-orm:schema-migration-revision (car (last chain))))))

(defun %next-id (dir)
  (format nil "~4,'0D" (1+ (hash-table-count (directory-by-id dir)))))

(defun make-revision (dir from-snapshot to-snapshot
                      &key id down-id name
                        (drop-tables nil) (drop-columns t))
  "Autogenerate a SCHEMA-MIGRATION from snapshots and register it.
   DOWN-ID defaults to the current head (NIL if empty). The autogenerate
   policy is SQL-ORM:MAKE-MIGRATION's (no table drops unless :DROP-TABLES),
   not the engine's complete diff."
  (let* ((rev (or id (%next-id dir)))
         (parent (if down-id
                     down-id
                     (head-revision dir)))
         (mig (sql-orm:make-migration from-snapshot to-snapshot
                                      :name name
                                      :revision rev
                                      :down-revision parent
                                      :drop-tables drop-tables
                                      :drop-columns drop-columns)))
    (register-revision dir mig)))

;;; ------------------------------------------------------------------
;;; Planning (S2)
;;; ------------------------------------------------------------------

(defmethod rev:plan-transition ((dir script-directory) target to)
  "S2 (linear graphs). TO: a revision designator, or NIL for the empty state
(plan-goal NIL). More than one head in DIR → BRANCH-NOT-SUPPORTED (exactly that
class, not MULTIPLE-HEADS), before TARGET is read. Steps: :DOWN from the
target's revision to the merge-base, then :UP to TO, one edge each. An :UP
step to R has STEP-DIFF = (REVISION-DELTA DIR R), the stored SCHEMA-DIFF (EQ);
a :DOWN step from R has (INVERT-DIFF (REVISION-DELTA DIR R)). Every step is
STEP-REVERSIBLE-P."
  (declare (ignore target to))
  (%not-implemented 'plan-transition))

(defun step-lossy-p (step)
  "True when STEP's diff is DIFF-LOSSY-P: running it drops a table or column,
or re-creates one whose data a drop lost. Data for approval; UPGRADE and
DOWNGRADE do not refuse lossy steps (0.1 behaviour)."
  (declare (ignore step))
  (%not-implemented 'step-lossy-p))

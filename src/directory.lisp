(in-package #:sql-migrate)

;;; SCRIPT-DIRECTORY (S2): a revision-protocol in-memory store with
;;; :PRIMARY :DELTA and a SCHEMA-DIFF-ENGINE. Each registered migration is
;;; one revision: id = :REVISION, parents = (:DOWN-REVISION) or none,
;;; content = (MAKE-SCHEMA-DIFF (MIGRATION-UPGRADE-OPS m)), so
;;; STACK-REV:REVISION-STATE folds the ops from the root with
;;; SQL-ORM:APPLY-OPS-TO-SNAPSHOT.
;;;
;;; BY-ID is an index id → the registered SCHEMA-MIGRATION object (so
;;; FIND-REVISION is an exact lookup returning it EQ); the graph itself lives
;;; in the store (COMMIT-REVISION, HEADS, WALK-REVISIONS).

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
unique in DIR (else MIGRATE-ERROR). DOWN-REVISION must be NIL or a registered
id (else UNKNOWN-REVISION, and nothing is stored); the migration is committed
to DIR as a revision whose content is a SCHEMA-DIFF of its upgrade ops. Forks
are accepted (S3 plans them)."
  (check-type dir script-directory)
  (check-type migration sql-orm:schema-migration)
  (let ((id (sql-orm:schema-migration-revision migration))
        (down (sql-orm:schema-migration-down-revision migration)))
    (unless (and (stringp id) (plusp (length id)))
      (error 'migrate-error :message "migration needs a non-empty :revision"))
    (when (find-revision dir id)
      (error 'migrate-error
             :revision id
             :message (format nil "duplicate revision ~S" id)))
    (unless (typep down '(or null string))
      (error 'migrate-error
             :revision id
             :message (format nil "down-revision ~S: merge revisions are not supported (S3)"
                              down)))
    (when (and down (not (find-revision dir down)))
      (error 'unknown-revision
             :revision down
             :designator down
             :message (format nil "down-revision ~S of ~S is not registered" down id)))
    (rev:commit-revision dir (make-schema-diff migration :engine (rev:content-engine dir))
                         :id id
                         :parents (and down (list down))
                         :message (sql-orm:schema-migration-name migration))
    (setf (gethash id (directory-by-id dir)) migration)
    migration))

(defun find-revision (dir id)
  "The SCHEMA-MIGRATION registered under exactly ID, or NIL. No id-prefix
matching (unlike STACK-REV:RESOLVE-REVISION on DIR)."
  (check-type dir script-directory)
  (values (gethash id (directory-by-id dir))))

(defun require-revision (dir id)
  "FIND-REVISION or signal UNKNOWN-REVISION (restart USE-VALUE: another id)."
  (or (find-revision dir id)
      (restart-case
          (error 'unknown-revision
                 :revision id
                 :designator id
                 :message (format nil "unknown revision ~S" id))
        (use-value (replacement)
          :report "Use another revision id"
          (require-revision dir replacement)))))

(defun %head-ids (dir)
  "Ids of DIR's heads, sorted STRING<."
  (sort (mapcar #'rev:revision-id (rev:heads dir)) #'string<))

(defun history (dir)
  "SCHEMA-MIGRATION list, parents before children (root to head on a linear
graph): the order of STACK-REV:WALK-REVISIONS :TOPO from every head. A forked
directory is listed too; never signals BRANCH-NOT-SUPPORTED."
  (check-type dir script-directory)
  (let ((heads (%head-ids dir)))
    (and heads
         (mapcar (lambda (r) (find-revision dir (rev:revision-id r)))
                 (rev:walk-revisions dir heads :order :topo)))))

(defun head-revision (dir)
  "Revision id of the unique head, or NIL if empty. Several heads →
MULTIPLE-HEADS (heads sorted STRING<) with restart USE-VALUE (an id, returned
as is)."
  (check-type dir script-directory)
  (let ((heads (%head-ids dir)))
    (if (rest heads)
        (restart-case
            (error 'multiple-heads
                   :heads heads
                   :message (format nil "the directory has ~D heads ~S" (length heads) heads))
          (use-value (id)
            :report "Use this revision id as the head"
            id))
        (first heads))))

(defun %next-id (dir)
  (format nil "~4,'0D" (1+ (hash-table-count (directory-by-id dir)))))

(defun make-revision (dir from-snapshot to-snapshot
                      &key id down-id name
                        (drop-tables nil) (drop-columns t))
  "Autogenerate a SCHEMA-MIGRATION from snapshots and register it.
   DOWN-ID defaults to the current head (NIL if empty; MULTIPLE-HEADS on a
   fork). The autogenerate policy is SQL-ORM:MAKE-MIGRATION's (no table drops
   unless :DROP-TABLES), not the engine's complete diff."
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

(defun %parent-id (dir id)
  "First parent id of revision ID, or NIL for a root."
  (first (rev:revision-parent-ids (rev:resolve-revision dir id))))

(defun %chain-to (dir id base)
  "(ID, parent, …) down to, not including, BASE (NIL: through the root)."
  (loop for x = id then (%parent-id dir x)
        until (equal x base)
        collect x))

(defun %up-step (dir id)
  (make-instance 'rev:plan-step
                 :direction :up :from (%parent-id dir id) :to id
                 :diff (rev:revision-delta dir id)
                 :reversible-p t))

(defun %down-step (dir id)
  (make-instance 'rev:plan-step
                 :direction :down :from id :to (%parent-id dir id)
                 :diff (diff:invert-diff (rev:revision-delta dir id))
                 :reversible-p t))

(defmethod rev:plan-transition ((dir script-directory) target to)
  "S2 (linear graphs). TO: a revision designator, or NIL for the empty state
(plan-goal NIL). More than one head in DIR → BRANCH-NOT-SUPPORTED (exactly that
class, not MULTIPLE-HEADS), before TARGET is read. Steps: :DOWN from the
target's revision to the merge-base, then :UP to TO, one edge each. An :UP
step to R has STEP-DIFF = (REVISION-DELTA DIR R), the stored SCHEMA-DIFF (EQ);
a :DOWN step from R has (INVERT-DIFF (REVISION-DELTA DIR R)). Every step is
STEP-REVERSIBLE-P."
  (let ((heads (%head-ids dir)))
    (when (rest heads)
      (error 'branch-not-supported
             :message (format nil "the directory has ~D heads ~S; branched plans are not supported (S3)"
                              (length heads) heads))))
  (let ((start (rev:target-revisions target)))
    (when (rest start)
      (error 'rev:revision-error
             :message (format nil "target is stamped with several revisions ~S; multi-head plans are not supported"
                              start)))
    (let* ((goal (and to (rev:revision-id (rev:resolve-revision dir to))))
           (from (and start (rev:revision-id (rev:resolve-revision dir (first start)))))
           (merge (and from goal (rev:merge-base dir from goal)))
           (base (and merge (rev:revision-id merge)))
           (down (and from (%chain-to dir from base)))
           (up (and goal (reverse (%chain-to dir goal base)))))
      (make-instance 'rev:transition-plan
                     :store dir
                     :start (copy-list start)
                     :goal (and goal (list goal))
                     :base base
                     :steps (append (mapcar (lambda (id) (%down-step dir id)) down)
                                    (mapcar (lambda (id) (%up-step dir id)) up))))))

(defun step-lossy-p (step)
  "True when STEP's diff is DIFF-LOSSY-P: running it drops a table or column,
or re-creates one whose data a drop lost. Data for approval; UPGRADE and
DOWNGRADE do not refuse lossy steps (0.1 behaviour)."
  (diff-lossy-p (rev:step-diff step)))

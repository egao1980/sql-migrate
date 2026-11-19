(in-package #:sql-migrate)

;;; SCHEMA-DIFF-ENGINE (S2): a diff-protocol engine whose states are sql-orm
;;; snapshots ((table-name . column-infos) …) and whose changes wrap one
;;; sql-orm SCHEMA-OP each. Brief: cl-stack docs/capabilities/sql.md,
;;; "sql-migrate 0.2.0 — S2 locked" → "Schema diff engine".
;;;
;;; Locked semantics (summary; the brief is normative):
;;;   COMPUTE-DIFF e a b  = the ops of (DIFF-SCHEMA a b :COMPLETE T :DIALECT d),
;;;                         one SCHEMA-CHANGE per op, in that order, at every
;;;                         level (DIFF-LEVEL records the level asked for).
;;;   Kinds               create-table / add-column → :ADD; drop-table /
;;;                       drop-column → :REMOVE; alter-column → :MODIFY.
;;;   Path                ("<t>") for table ops, ("<t>" "<c>") for column
;;;                       ops, names downcased strings.
;;;   Footprint           SCHEMA-OP-FOOTPRINT of the op.
;;;   Before / after      table ops: the table entry (name . column-infos);
;;;                       column ops: the column-info; alter: from / to.
;;;   Lossy               every :REMOVE; and every inverse of a lossy change.
;;;   STATE-EQUAL         SCHEMA-SNAPSHOT-EQUAL :DIALECT d.
;;;   APPLY-DIFF          APPLY-OPS-TO-SNAPSHOT per change, in order, pure; a
;;;                       misfit op → DIFF:APPLY-CONFLICT (:EXISTS / :MISSING)
;;;                       with SKIP-CHANGE and ABORT.
;;;   INVERT-DIFF         INVERT-SCHEMA-OP per change, order reversed; never
;;;                       signals IRREVERSIBLE-CHANGE.
;;;   COMPOSE-DIFFS       concatenation → SCHEMA-DIFF.
;;;   RENDER-DIFF :SQL    offline DDL in the engine's dialect, one statement
;;;                       per line ending in ";".

(defclass schema-diff-engine (diff:diff-engine)
  ((dialect :initarg :dialect :initform nil :reader schema-engine-dialect
            :documentation "NIL (ANSI), a dialect keyword (:SQLITE3,
:POSTGRES) or a SQL-QUERY:SQL-DIALECT instance; decides column equality and
the :SQL render."))
  (:documentation "diff-protocol engine over sql-orm schema snapshots.
ENGINE-NAME → :SCHEMA."))

(defun make-schema-diff-engine (&key dialect)
  "A SCHEMA-DIFF-ENGINE comparing and rendering under DIALECT (see
SQL-ORM:SCHEMA-DIALECT-KEY for designators)."
  (make-instance 'schema-diff-engine :dialect dialect))

(defclass schema-diff (diff:diff) ()
  (:documentation "A DIFF-PROTOCOL:DIFF whose changes are SCHEMA-CHANGEs.
COMPUTE-DIFF, INVERT-DIFF and COMPOSE-DIFFS on the schema engine return one."))

;;; A SCHEMA-CHANGE keeps its before / after sides in its own slots (both
;;; always loaded) and answers diff-protocol's exported readers from them.

(defclass schema-change (diff:change)
  ((op :initarg :op :reader change-schema-op
       :documentation "The SQL-ORM:SCHEMA-OP this change applies.")
   (lossy :initarg :lossy :initform nil :reader change-lossy-p
          :documentation "True when applying the change loses data, or
undoes a change that did: drops, and inverses of lossy changes.")
   (before :initarg :before-value :initform nil :reader %schema-change-before)
   (after :initarg :after-value :initform nil :reader %schema-change-after))
  (:documentation "One schema change: a DIFF-PROTOCOL:CHANGE carrying its
SCHEMA-OP and a lossy flag."))

(defmethod diff:change-before ((c schema-change)) (%schema-change-before c))
(defmethod diff:change-after ((c schema-change)) (%schema-change-after c))
(defmethod diff:change-before-loaded-p ((c schema-change)) t)
(defmethod diff:change-after-loaded-p ((c schema-change)) t)

(defun %name-string (name)
  (string-downcase (string name)))

(defun %table-entry (op)
  (cons (sql-orm:schema-op-table-name op) (sql-orm:schema-op-columns op)))

(defun %op-shape (op)
  "→ (values kind path before after) of the change wrapping OP."
  (let* ((table (%name-string (sql-orm:schema-op-table-name op)))
         (column-path (lambda () (list table (%name-string (sql-orm:schema-op-column-name op))))))
    (etypecase op
      (sql-orm:create-table-op (values :add (list table) nil (%table-entry op)))
      (sql-orm:drop-table-op (values :remove (list table) (%table-entry op) nil))
      (sql-orm:add-column-op
       (values :add (funcall column-path) nil (sql-orm:schema-op-column op)))
      (sql-orm:drop-column-op
       (values :remove (funcall column-path) (sql-orm:schema-op-column op) nil))
      (sql-orm:alter-column-op
       (values :modify (funcall column-path)
               (sql-orm:schema-op-from-column op) (sql-orm:schema-op-to-column op))))))

(defun %op->change (op &key inherited-lossy)
  "SCHEMA-CHANGE wrapping OP. Lossy when OP removes something, or when
INHERITED-LOSSY (OP inverts a lossy change)."
  (multiple-value-bind (kind path before after) (%op-shape op)
    (make-instance 'schema-change
                   :kind kind :path path
                   :footprint (sql-orm:schema-op-footprint op)
                   :op op
                   :lossy (or (and inherited-lossy t) (eq kind :remove))
                   :before-value before :after-value after)))

(defun %schema-diff (changes engine level)
  (make-instance 'schema-diff :changes changes :engine engine :level level))

(defun %coerce-ops (ops)
  (etypecase ops
    (sql-orm:schema-migration (sql-orm:migration-upgrade-ops ops))
    (list ops)
    (sql-orm:schema-op (list ops))))

(defun make-schema-diff (ops &key (engine (make-schema-diff-engine)) (level :changes))
  "→ SCHEMA-DIFF with one SCHEMA-CHANGE per op of OPS (a list of SCHEMA-OP,
a single op, or a SCHEMA-MIGRATION's upgrade ops), in order. ENGINE defaults
to a fresh ANSI SCHEMA-DIFF-ENGINE. Kinds, paths, footprints and lossy flags
as COMPUTE-DIFF makes them."
  (check-type engine schema-diff-engine)
  (check-type level diff:diff-level)
  (%schema-diff (mapcar #'%op->change (%coerce-ops ops)) engine level))

(defgeneric schema-diff-ops (diff)
  (:documentation "→ fresh list of the SCHEMA-OPs of DIFF's changes, in apply
order. Applying them with SQL-ORM:UPGRADE-SCHEMA is what EXECUTE-PLAN runs
for a step."))

(defmethod schema-diff-ops ((d diff:diff))
  (mapcar #'change-schema-op (diff:diff-changes d)))

(defgeneric diff-lossy-p (diff)
  (:documentation "True when some change of DIFF is CHANGE-LOSSY-P."))

(defmethod diff-lossy-p ((d diff:diff))
  (and (some (lambda (c) (and (typep c 'schema-change) (change-lossy-p c)))
             (diff:diff-changes d))
       t))

(defmethod diff-lossy-p ((d null))
  nil)

;;; diff-protocol methods

(defmethod diff:engine-name ((e schema-diff-engine))
  :schema)

(defmethod diff:compute-diff ((e schema-diff-engine) a b &key (level :changes))
  (make-schema-diff (sql-orm:diff-schema a b :complete t :dialect (schema-engine-dialect e))
                    :engine e :level level))

(defmethod diff:state-equal ((e schema-diff-engine) a b)
  (sql-orm:schema-snapshot-equal a b :dialect (schema-engine-dialect e)))

(defun %conflict-reason (op state)
  ":EXISTS for a create / add of a name STATE already holds, else :MISSING."
  (typecase op
    (sql-orm:create-table-op :exists)
    (sql-orm:add-column-op
     (if (find (sql-orm:schema-op-table-name op) state :key #'car :test #'string-equal)
         :exists
         :missing))
    (t :missing)))

(defmethod diff:apply-diff (state (d schema-diff))
  (restart-case
      ;; A fresh copy even with no changes: the result never aliases STATE.
      (let ((current (sql-orm:apply-ops-to-snapshot state '()))
            (skipped '()))
        (dolist (c (diff:diff-changes d) (values current (nreverse skipped)))
          (let ((op (change-schema-op c)))
            (restart-case
                (setf current
                      ;; HANDLER-CASE unwinds out of sql-orm first, so the
                      ;; restarts below never resume inside its fold.
                      (handler-case (sql-orm:apply-ops-to-snapshot current (list op))
                        (sql-orm:snapshot-conflict ()
                          (error 'diff:apply-conflict
                                 :diff d :change c
                                 :reason (%conflict-reason op current)))))
              (diff:skip-change ()
                :report "Leave this change out and continue."
                (push c skipped))))))
    (abort ()
      :report "Abort APPLY-DIFF and return the original state."
      (values state :aborted))))

(defmethod diff:invert-diff ((d schema-diff))
  (%schema-diff (reverse
                 (mapcar (lambda (c)
                           (%op->change (sql-orm:invert-schema-op (change-schema-op c))
                                        :inherited-lossy (change-lossy-p c)))
                         (diff:diff-changes d)))
                (diff:diff-engine d)
                (diff:diff-level d)))

(defmethod diff:compose-diffs ((d1 schema-diff) (d2 schema-diff))
  (%schema-diff (append (diff:diff-changes d1) (diff:diff-changes d2))
                (diff:diff-engine d1)
                (diff:diff-level d1)))

(defun %render-dialect (designator)
  "SQL-QUERY dialect instance for an engine dialect designator. NIL is a fresh
ANSI dialect, never SQL-QUERY:*SQL-DIALECT*."
  (etypecase designator
    (null (sql-query:make-ansi-dialect))
    (keyword (or (gethash designator sql-query:*sql-dialect-registry*)
                 (and (eq designator :ansi) (sql-query:make-ansi-dialect))
                 (error 'diff:unsupported-content
                        :content designator)))
    (sql-query:sql-dialect designator)))

(defmethod diff:render-diff ((d schema-diff) (format (eql :sql)) stream)
  (let* ((engine (diff:diff-engine d))
         (designator (and engine (schema-engine-dialect engine)))
         (dialect (%render-dialect designator))
         (key (sql-orm:schema-dialect-key designator))
         ;; Built in full first: a refusal writes nothing to STREAM.
         (text (with-output-to-string (out)
                 (dolist (c (diff:diff-changes d))
                   (let ((op (change-schema-op c)))
                     (when (and (eq key :sqlite3) (typep op 'sql-orm:alter-column-op))
                       (error 'diff:unsupported-content
                              :diff d :change c :engine engine :content op))
                     (dolist (statement (sql-orm:schema-op-upgrade op))
                       (cl:format out "~A;~%"
                                  (values (sql-query:compile-sql statement :dialect dialect)))))))))
    (cond ((null stream) text)
          ((eq stream t) (write-string text *standard-output*) nil)
          (t (write-string text stream) nil))))

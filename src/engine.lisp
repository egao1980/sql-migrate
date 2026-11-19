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

(defclass schema-change (diff:change)
  ((op :initarg :op :reader change-schema-op
       :documentation "The SQL-ORM:SCHEMA-OP this change applies.")
   (lossy :initarg :lossy :initform nil :reader change-lossy-p
          :documentation "True when applying the change loses data, or
undoes a change that did: drops, and inverses of lossy changes."))
  (:documentation "One schema change: a DIFF-PROTOCOL:CHANGE carrying its
SCHEMA-OP and a lossy flag."))

(defun make-schema-diff (ops &key engine (level :changes))
  "→ SCHEMA-DIFF with one SCHEMA-CHANGE per op of OPS (a list of SCHEMA-OP,
a single op, or a SCHEMA-MIGRATION's upgrade ops), in order. ENGINE defaults
to a fresh ANSI SCHEMA-DIFF-ENGINE. Kinds, paths, footprints and lossy flags
as COMPUTE-DIFF makes them."
  (declare (ignore ops engine level))
  (%not-implemented 'make-schema-diff))

(defgeneric schema-diff-ops (diff)
  (:documentation "→ fresh list of the SCHEMA-OPs of DIFF's changes, in apply
order. Applying them with SQL-ORM:UPGRADE-SCHEMA is what EXECUTE-PLAN runs
for a step."))

(defmethod schema-diff-ops ((d diff:diff))
  (%not-implemented 'schema-diff-ops))

(defgeneric diff-lossy-p (diff)
  (:documentation "True when some change of DIFF is CHANGE-LOSSY-P."))

(defmethod diff-lossy-p ((d diff:diff))
  (%not-implemented 'diff-lossy-p))

(defmethod diff-lossy-p ((d null))
  nil)

;;; diff-protocol methods

(defmethod diff:engine-name ((e schema-diff-engine))
  :schema)

(defmethod diff:compute-diff ((e schema-diff-engine) a b &key (level :changes))
  (declare (ignore a b level))
  (%not-implemented 'compute-diff))

(defmethod diff:state-equal ((e schema-diff-engine) a b)
  (declare (ignore a b))
  (%not-implemented 'state-equal))

(defmethod diff:apply-diff (state (d schema-diff))
  (declare (ignore state))
  (%not-implemented 'apply-diff))

(defmethod diff:invert-diff ((d schema-diff))
  (%not-implemented 'invert-diff))

(defmethod diff:compose-diffs ((d1 schema-diff) (d2 schema-diff))
  (%not-implemented 'compose-diffs))

(defmethod diff:render-diff ((d schema-diff) (format (eql :sql)) stream)
  (declare (ignore stream))
  (%not-implemented 'render-diff))

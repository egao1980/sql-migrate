(in-package #:sql-migrate/tests)

;;; S2 — SCHEMA-DIFF-ENGINE: diff-protocol laws over the snapshot corpus,
;;; change shapes, apply conflicts, render :summary / :sql. No database.

;;; ---------------------------------------------------------------------------
;;; Laws (diff-protocol 1–4) over the corpus, every level
;;; ---------------------------------------------------------------------------

(deftest-parametrize schema-engine-apply-roundtrip
    (id :ids (corpus-ids) :rows (corpus-ids))
  "apply-diff(a, compute-diff(a, b)) = b at every level; inputs untouched."
  (multiple-value-bind (a b) (corpus-pair id)
    (let ((e (engine))
          (a-copy (copy-snapshot a))
          (b-copy (copy-snapshot b)))
      (dolist (level *levels*)
        (let ((d (diff:compute-diff e a b :level level)))
          (ok (typep d 'schema-diff))
          (ok (eq level (diff:diff-level d)))
          (ok (eq e (diff:diff-engine d)))
          (ok (diff:state-equal e (diff:apply-diff a d) b))
          (ok (schema-snapshot-equal (diff:apply-diff a d) b))))
      (ok (equalp a a-copy) "A not mutated")
      (ok (equalp b b-copy) "B not mutated"))))

(deftest-parametrize schema-engine-inverse-roundtrip
    (id :ids (corpus-ids) :rows (corpus-ids))
  "apply-diff(b, invert-diff(compute-diff(a, b))) = a at every level."
  (multiple-value-bind (a b) (corpus-pair id)
    (let ((e (engine)))
      (dolist (level *levels*)
        (let* ((d (diff:compute-diff e a b :level level))
               (inv (diff:invert-diff d)))
          (ok (typep inv 'schema-diff))
          (ok (diff:state-equal e (diff:apply-diff b inv) a))
          (ok (equal (diff:diff-footprint d) (diff:diff-footprint inv))
              "an op and its inverse have the same footprint")
          (ok (diff:state-equal e (diff:apply-diff a (diff:invert-diff inv)) b)
              "double inverse applies like the original"))))))

(deftest-parametrize schema-engine-self-diff-empty
    (id :ids (corpus-ids) :rows (corpus-ids))
  (multiple-value-bind (a b) (corpus-pair id)
    (let ((e (engine)))
      (dolist (level *levels*)
        (ok (diff:diff-empty-p (diff:compute-diff e a a :level level)))
        (ok (diff:diff-empty-p (diff:compute-diff e b (copy-snapshot b) :level level)))))))

(deftest-parametrize schema-engine-compose
    (id :ids (corpus-ids) :rows (corpus-ids))
  "compose(d(a,b), d(b,a)) applied to a = a; composition is associative."
  (multiple-value-bind (a b) (corpus-pair id)
    (let* ((e (engine))
           (ab (diff:compute-diff e a b))
           (ba (diff:compute-diff e b a))
           (there-and-back (diff:compose-diffs ab ba)))
      (ok (typep there-and-back 'schema-diff))
      (ok (diff:state-equal e (diff:apply-diff a there-and-back) a))
      (ok (= (length (diff:diff-changes there-and-back))
             (+ (length (diff:diff-changes ab)) (length (diff:diff-changes ba)))))
      (ok (diff:state-equal e
                            (diff:apply-diff a (diff:compose-diffs (diff:compose-diffs ab ba) ab))
                            (diff:apply-diff a (diff:compose-diffs ab (diff:compose-diffs ba ab)))))
      (ok (diff:state-equal e
                            (diff:apply-diff a (diff:compose-diffs ab (diff:compose-diffs ba ab)))
                            b)))))

;;; ---------------------------------------------------------------------------
;;; Change shapes
;;; ---------------------------------------------------------------------------

(deftest-parametrize schema-engine-change-shape
    (id :ids (corpus-ids) :rows (corpus-ids))
  "One SCHEMA-CHANGE per op of diff-schema :complete t: kinds, ops, footprints, lossy."
  (destructuring-bind (a b kinds lossy) (corpus-shape id)
    (let* ((e (engine))
           (ops (diff-schema a b :complete t))
           (d (diff:compute-diff e a b)))
      (ok (every (lambda (c) (typep c 'schema-change)) (diff:diff-changes d)))
      (ok (equal kinds (change-kinds d)))
      (ok (equal (op-classes ops)
                 (op-classes (mapcar #'change-schema-op (diff:diff-changes d)))))
      (ok (equal (op-classes ops) (op-classes (schema-diff-ops d))))
      (ok (equal (mapcar #'schema-op-footprint ops)
                 (mapcar #'diff:change-footprint (diff:diff-changes d))))
      (ok (equal (sorted-union (mapcar #'schema-op-footprint ops))
                 (diff:diff-footprint d)))
      (ok (equal lossy (change-lossy-flags d)))
      (ok (eq (and (member t lossy) t) (and (diff-lossy-p d) t)))
      ;; Inverse: kinds swap, order reverses; lossy for every add / remove
      ;; (a drop, or the re-creation of something a drop emptied).
      (let ((inv (diff:invert-diff d)))
        (ok (equal (reverse (mapcar (lambda (k) (case k (:add :remove) (:remove :add) (t k)))
                                    kinds))
                   (change-kinds inv)))
        (ok (equal (reverse (mapcar (lambda (k) (and (member k '(:add :remove)) t)) kinds))
                   (change-lossy-flags inv)))))))

(deftest schema-engine-paths
  "Paths are downcased name strings: (\"<t>\") for table ops, (\"<t>\" \"<c>\") for column ops."
  (let ((e (engine)))
    (multiple-value-bind (a b) (corpus-pair "multi-table")
      (ok (equal '(("items" "sku") ("items" "qty") ("tags" "label") ("tags" "color")
                   ("users") ("notes"))
                 (mapcar #'diff:change-path (diff:diff-changes (diff:compute-diff e a b))))))
    (ok (equal '(("items"))
               (mapcar #'diff:change-path
                       (diff:diff-changes (diff:compute-diff e nil (list (cons "ITEMS" (cdr (items-v1)))))))))))

(deftest schema-engine-before-after
  (let ((e (engine)))
    (multiple-value-bind (a b) (corpus-pair "type-change")
      (let ((c (first (diff:diff-changes (diff:compute-diff e a b)))))
        (ok (typep (change-schema-op c) 'alter-column-op))
        (ok (string-equal "qty" (column-info-name (diff:change-before c))))
        (ok (eq :integer (column-info-type (diff:change-before c))))
        (ok (eq :text (column-info-type (diff:change-after c))))))
    (multiple-value-bind (a b) (corpus-pair "add-column")
      (let ((c (first (diff:diff-changes (diff:compute-diff e a b)))))
        (ok (null (diff:change-before c)))
        (ok (string-equal "sku" (column-info-name (diff:change-after c))))))
    (multiple-value-bind (a b) (corpus-pair "drop-table")
      (let ((c (first (diff:diff-changes (diff:compute-diff e a b)))))
        (ok (string-equal "items" (string (car (diff:change-before c)))))
        (ok (= 3 (length (cdr (diff:change-before c)))))
        (ok (null (diff:change-after c)))))))

(deftest schema-engine-name-and-dialect
  (ok (eq :schema (diff:engine-name (engine))))
  (ok (null (schema-engine-dialect (engine))))
  (ok (eq :sqlite3 (schema-engine-dialect (make-schema-diff-engine :dialect :sqlite3))))
  ;; BOOLEAN and INTEGER are both INTEGER on SQLite, not in ANSI.
  (let ((a (list (tbl :flags (col "id" :integer :primary-key t) (col "on" :boolean))))
        (b (list (tbl :flags (col "id" :integer :primary-key t) (col "on" :integer))))
        (lite (make-schema-diff-engine :dialect :sqlite3)))
    (ok (not (diff:state-equal (engine) a b)))
    (ok (diff:state-equal lite a b))
    (ok (diff:diff-empty-p (diff:compute-diff lite a b)))
    (ok (equal '(:modify) (change-kinds (diff:compute-diff (engine) a b))))))

(deftest schema-engine-make-schema-diff
  "MAKE-SCHEMA-DIFF wraps given ops (or a migration) with the same shapes."
  (let* ((v1 (%snap-v1))
         (v2 (%snap-v2))
         (mig (make-migration v1 v2 :revision "x"))
         (d (make-schema-diff (migration-upgrade-ops mig)))
         (dm (make-schema-diff mig)))
    (ok (typep d 'schema-diff))
    (ok (typep (diff:diff-engine d) 'schema-diff-engine))
    (ok (null (schema-engine-dialect (diff:diff-engine d))))
    (ok (eq :changes (diff:diff-level d)))
    (ok (equal '(:add) (change-kinds d)))
    (ok (equal '("column:widgets.color") (diff:diff-footprint d)))
    (ok (equal '(add-column-op) (op-classes (schema-diff-ops dm))))
    (ok (null (diff-lossy-p d)))
    (ok (diff-lossy-p (diff:invert-diff d)))
    (ok (diff:diff-empty-p (make-schema-diff '())))))

;;; ---------------------------------------------------------------------------
;;; Apply conflicts (restarts recorded inside the handler)
;;; ---------------------------------------------------------------------------

(deftest schema-engine-apply-conflict-exists
  (let* ((e (engine))
         (v (list (items-v1)))
         (d (diff:compute-diff e nil v))
         (seen nil))
    (multiple-value-bind (state skipped)
        (handler-bind ((diff:apply-conflict
                         (lambda (c)
                           (setf seen (list (diff:apply-conflict-reason c)
                                            (restart-names c)))
                           (invoke-restart 'diff:skip-change))))
          (diff:apply-diff v d))
      (ok (eq :exists (first seen)))
      (ok (member 'diff:skip-change (second seen)))
      (ok (member 'abort (second seen)))
      (ok (schema-snapshot-equal state v))
      (ok (= 1 (length skipped)))
      (ok (typep (first skipped) 'schema-change)))))

(deftest schema-engine-apply-conflict-missing-abort
  (let* ((e (engine))
         (v (list (items-v1)))
         (d (diff:compute-diff e v nil))
         (reason nil))
    (ok (signals (diff:apply-diff nil d) 'diff:apply-conflict))
    (multiple-value-bind (state flag)
        (handler-bind ((diff:apply-conflict
                         (lambda (c)
                           (setf reason (diff:apply-conflict-reason c))
                           (invoke-restart 'abort))))
          (diff:apply-diff (list (notes-v1)) d))
      (ok (eq :missing reason))
      (ok (eq :aborted flag))
      (ok (equal '(:notes) (mapcar #'car state))))))

;;; ---------------------------------------------------------------------------
;;; Rendering
;;; ---------------------------------------------------------------------------

(deftest schema-engine-render-summary
  "The inherited :summary render: kind letter + footprint per change."
  (let* ((e (engine))
         (v1 (%snap-v1))
         (v2 (%snap-v2)))
    (ok (equal (format nil "A table:widgets~%")
               (diff:render-diff (diff:compute-diff e nil v1) :summary nil)))
    (ok (equal (format nil "A column:widgets.color~%")
               (diff:render-diff (diff:compute-diff e v1 v2) :summary nil)))
    (ok (equal (format nil "D table:widgets~%")
               (diff:render-diff (diff:compute-diff e v2 nil) :summary nil)))))

(defun %lite () (make-schema-diff-engine :dialect :sqlite3))
(defun %pg () (make-schema-diff-engine :dialect :postgres))

(deftest schema-engine-render-sql-sqlite
  (let ((v1 (%snap-v1))
        (v2 (%snap-v2)))
    (ok (equal (concatenate 'string
                            "CREATE TABLE \"widgets\" (\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, "
                            "\"name\" TEXT NOT NULL);" (string #\Newline))
               (diff:render-diff (diff:compute-diff (%lite) nil v1) :sql nil)))
    (ok (equal (format nil "ALTER TABLE \"widgets\" ADD COLUMN \"color\" TEXT;~%")
               (diff:render-diff (diff:compute-diff (%lite) v1 v2) :sql nil)))
    (ok (equal (format nil "ALTER TABLE \"widgets\" DROP COLUMN \"color\";~%")
               (diff:render-diff (diff:invert-diff (diff:compute-diff (%lite) v1 v2)) :sql nil)))
    (ok (equal (format nil "DROP TABLE IF EXISTS \"widgets\";~%")
               (diff:render-diff (diff:compute-diff (%lite) v2 nil) :sql nil)))
    (ok (equal "" (diff:render-diff (diff:compute-diff (%lite) v1 v1) :sql nil)))))

(deftest schema-engine-render-sql-dialects
  (let ((v1 (%snap-v1)))
    (ok (equal (concatenate 'string
                            "CREATE TABLE \"widgets\" (\"id\" SERIAL PRIMARY KEY NOT NULL, "
                            "\"name\" TEXT NOT NULL);" (string #\Newline))
               (diff:render-diff (diff:compute-diff (%pg) nil v1) :sql nil)))
    (ok (equal (concatenate 'string
                            "CREATE TABLE \"widgets\" (\"id\" INTEGER GENERATED BY DEFAULT AS IDENTITY "
                            "PRIMARY KEY NOT NULL, \"name\" CHARACTER VARYING NOT NULL);" (string #\Newline))
               (diff:render-diff (diff:compute-diff (engine) nil v1) :sql nil))
        "NIL dialect renders ANSI")))

(deftest schema-engine-render-sql-alter
  "Portable ALTER COLUMN on PostgreSQL; SQLite needs a live rebuild → unsupported offline."
  (let ((a (list (tbl :widgets (col "id" :integer :primary-key t) (col "color" :text))))
        (b (list (tbl :widgets (col "id" :integer :primary-key t)
                      (col "color" :text :not-null t :default "red")))))
    (ok (equal (format nil "ALTER TABLE \"widgets\" ALTER COLUMN \"color\" SET NOT NULL;~%ALTER TABLE \"widgets\" ALTER COLUMN \"color\" SET DEFAULT 'red';~%")
               (diff:render-diff (diff:compute-diff (%pg) a b) :sql nil)))
    (ok (signals (diff:render-diff (diff:compute-diff (%lite) a b) :sql nil)
                 'diff:unsupported-content))))

(deftest schema-engine-render-sql-streams
  (let* ((d (diff:compute-diff (%lite) (%snap-v1) (%snap-v2)))
         (expected (format nil "ALTER TABLE \"widgets\" ADD COLUMN \"color\" TEXT;~%"))
         (to-t :unset)
         (to-s :unset)
         (out-t (with-output-to-string (*standard-output*)
                  (setf to-t (diff:render-diff d :sql t))))
         (out-s (with-output-to-string (s)
                  (setf to-s (diff:render-diff d :sql s)))))
    (ok (equal expected out-t) "T writes to *standard-output*")
    (ok (null to-t) "… and returns NIL")
    (ok (equal expected out-s))
    (ok (null to-s))))

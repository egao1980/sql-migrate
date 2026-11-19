(in-package #:sql-migrate/tests)

;;; sql-migrate 0.2.0 S2 fixtures: snapshot builders, a snapshot-pair corpus
;;; (the sql-orm S1 corpus ideas, restated with the change shapes the schema
;;; engine must produce), directory builders, and SQLite helpers.
;;; Snapshots are built fresh on every call.

;;; ---------------------------------------------------------------------------
;;; Builders
;;; ---------------------------------------------------------------------------

(defun col (name type &rest keys &key primary-key autoincrement not-null unique default)
  "COLUMN-INFO named NAME (sql-orm compares names STRING-EQUAL)."
  (declare (ignore primary-key autoincrement not-null unique default))
  (apply #'sql-orm::make-column-info :name name :type type keys))

(defun tbl (name &rest columns)
  (cons name columns))

(defun copy-snapshot (snapshot)
  "Deep copy: fresh conses and fresh COLUMN-INFOs."
  (mapcar (lambda (entry)
            (cons (car entry) (mapcar #'sql-orm::copy-column-info (cdr entry))))
          snapshot))

(defun op-classes (ops)
  (mapcar (lambda (op) (class-name (class-of op))) ops))

(defun change-kinds (diff)
  (mapcar #'diff:change-kind (diff:diff-changes diff)))

(defun change-lossy-flags (diff)
  (mapcar (lambda (c) (and (change-lossy-p c) t)) (diff:diff-changes diff)))

(defun sorted-union (lists)
  (sort (remove-duplicates (copy-list (reduce #'append lists)) :test #'string=)
        #'string<))

(defun items-v1 ()
  (tbl :items
       (col "id" :integer :primary-key t :autoincrement t)
       (col "name" :text :not-null t)
       (col "qty" :integer)))

(defun items-with (&rest replacements)
  "ITEMS-V1 with columns replaced by name; a new name is appended."
  (let ((table (items-v1)))
    (dolist (r replacements table)
      (let ((pos (position (column-info-name r) (cdr table)
                           :key #'column-info-name :test #'string-equal)))
        (if pos
            (setf (nth pos (cdr table)) r)
            (setf (cdr table) (append (cdr table) (list r))))))))

(defun items-without (name)
  (let ((table (items-v1)))
    (cons (car table)
          (remove name (cdr table) :key #'column-info-name :test #'string-equal))))

(defun notes-v1 ()
  (tbl :notes
       (col "id" :integer :primary-key t :autoincrement t)
       (col "body" :text)))

(defun tags-v1 ()
  (tbl :tags
       (col "id" :integer :primary-key t)
       (col "label" (list :varchar 32))))

(defun users-v1 ()
  (tbl :users
       (col "id" :integer :primary-key t :autoincrement t)
       (col "email" :text :not-null t :unique t)
       (col "active" :boolean :default t)))

;;; ---------------------------------------------------------------------------
;;; Corpus
;;;
;;; Row: (id a b kinds lossy)
;;;   kinds  CHANGE-KIND of each change of (COMPUTE-DIFF e a b), in order —
;;;          one change per op of (DIFF-SCHEMA a b :COMPLETE T)
;;;   lossy  CHANGE-LOSSY-P of each change (true exactly for :REMOVE)
;;; ---------------------------------------------------------------------------

(defun corpus ()
  (list
   (list "create-table" '() (list (items-v1)) '(:add) '(nil))
   (list "drop-table" (list (items-v1)) '() '(:remove) '(t))
   (list "add-column" (list (items-v1)) (list (items-with (col "sku" :text)))
         '(:add) '(nil))
   (list "add-not-null-with-default" (list (items-v1))
         (list (items-with (col "stock" :integer :not-null t :default 0)))
         '(:add) '(nil))
   (list "drop-column" (list (items-v1)) (list (items-without "qty"))
         '(:remove) '(t))
   (list "type-change" (list (items-v1)) (list (items-with (col "qty" :text)))
         '(:modify) '(nil))
   (list "set-not-null" (list (items-v1))
         (list (items-with (col "qty" :integer :not-null t)))
         '(:modify) '(nil))
   (list "drop-not-null" (list (items-v1)) (list (items-with (col "name" :text)))
         '(:modify) '(nil))
   (list "set-default" (list (items-v1))
         (list (items-with (col "qty" :integer :default 0)))
         '(:modify) '(nil))
   (list "change-default" (list (items-with (col "qty" :integer :default 0)))
         (list (items-with (col "qty" :integer :default 1)))
         '(:modify) '(nil))
   (list "drop-default" (list (items-with (col "name" :text :not-null t :default "x")))
         (list (items-v1))
         '(:modify) '(nil))
   (list "alter-all" (list (items-v1))
         (list (items-with (col "qty" :real :not-null t :default 1)))
         '(:modify) '(nil))
   (list "varchar-length" (list (tags-v1))
         (list (tbl :tags (col "id" :integer :primary-key t)
                    (col "label" (list :varchar 64))))
         '(:modify) '(nil))
   (list "unique-toggle" (list (items-v1))
         (list (items-with (col "name" :text :not-null t :unique t)))
         '(:remove :add) '(t nil))
   (list "pk-toggle" (list (tags-v1))
         (list (tbl :tags (col "id" :integer) (col "label" (list :varchar 32))))
         '(:remove :add) '(t nil))
   (list "multi-table"
         (list (items-v1) (notes-v1) (tags-v1))
         (list (items-with (col "qty" :integer :default 0) (col "sku" :text))
               (tbl :tags (col "id" :integer :primary-key t) (col "color" :text))
               (users-v1))
         ;; add sku, alter qty | drop label, add color | create users | drop notes
         '(:add :modify :remove :add :add :remove)
         '(nil nil t nil nil t))
   (list "identity" (list (items-v1) (users-v1)) (list (items-v1) (users-v1)) '() '())
   (list "reorder-only" (list (items-v1) (notes-v1))
         (list (notes-v1)
               (tbl :items
                    (col "qty" :integer)
                    (col "id" :integer :primary-key t :autoincrement t)
                    (col "name" :text :not-null t)))
         '() '())
   (list "name-case" (list (items-v1))
         (list (cons "ITEMS" (mapcar (lambda (c)
                                       (let ((c (sql-orm::copy-column-info c)))
                                         (setf (column-info-name c)
                                               (string-upcase (column-info-name c)))
                                         c))
                                     (cdr (items-v1)))))
         '() '())))

;;; DEFTEST-PARAMETRIZE :ROWS splices row values into the test as forms, so
;;; parametrized tests take a corpus id (a self-evaluating string) and build
;;; the snapshots inside the body.

(defun corpus-ids ()
  (mapcar #'first (corpus)))

(defun %corpus-row (id)
  (or (find id (corpus) :key #'first :test #'string=)
      (error "no corpus row ~S" id)))

(defun corpus-pair (id)
  "Fresh (values a b) for corpus row ID."
  (let ((row (%corpus-row id)))
    (values (second row) (third row))))

(defun corpus-shape (id)
  "Fresh (a b kinds lossy) for corpus row ID."
  (rest (%corpus-row id)))

(defparameter *levels* '(:changes :summary :content))

(defun engine ()
  (make-schema-diff-engine))

;;; ---------------------------------------------------------------------------
;;; Directories
;;; ---------------------------------------------------------------------------

(defun linear-dir ()
  "0001 creates widgets (id, name); 0002 adds widgets.color — the 0.1 %DIR."
  (%dir))

(defun forked-dir ()
  "a creates widgets; b and c both add widgets.color on top of a: two heads."
  (let ((dir (make-script-directory))
        (v1 (%snap-v1))
        (v2 (%snap-v2)))
    (register-revision dir (make-migration nil v1 :revision "a" :down-revision nil))
    (register-revision dir (make-migration v1 v2 :revision "b" :down-revision "a"))
    (register-revision dir (make-migration v1 v2 :revision "c" :down-revision "a"))
    dir))

(defun memory-target (&rest revisions)
  "An in-memory STACK-REV target stamped REVISIONS: plans without a database."
  (rev:make-in-memory-target :revisions revisions))

;;; A REVISION-TARGET with no methods: any protocol call on it fails with
;;; NO-APPLICABLE-METHOD, so a test can show that a call never read it.
(defclass untouchable-target (rev:revision-target) ())

;;; ---------------------------------------------------------------------------
;;; SQLite helpers
;;; ---------------------------------------------------------------------------

(defmacro with-sqlite ((var) &body body)
  `(with-orm-connection (,var :driver :sqlite3 :database-name ":memory:")
     ,@body))

(defun table-exists-p (c name)
  (and (sql-protocol:fetch
        (sql-protocol:execute
         c "SELECT name FROM sqlite_master WHERE type = 'table' AND name = ?"
         (list name)))
       t))

(defun version-rows (c dir)
  "Version-table ids, sorted."
  (ensure-version-table c dir)
  (sort (mapcar (lambda (row) (getf row :version_num))
                (sql-protocol:fetch-all
                 (sql-protocol:execute
                  c (format nil "SELECT version_num FROM ~A"
                            (directory-version-table dir)))))
        #'string<))

(defun restart-names (condition)
  "Names of the restarts visible for CONDITION, recorded inside a handler
(after the signalling form returns no restart is left to find)."
  (remove nil (mapcar #'restart-name (compute-restarts condition))))

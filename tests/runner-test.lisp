(in-package #:sql-migrate/tests)

(defun %snap-v1 ()
  (clrhash sql-orm::*model-registry*)
  (defmodel widget-v1 ()
    (id :integer :primary-key t :autoincrement t)
    (name :text :not-null t)
    (:table widgets))
  (schema-snapshot '(widget-v1)))

(defun %snap-v2 ()
  (clrhash sql-orm::*model-registry*)
  (defmodel widget-v2 ()
    (id :integer :primary-key t :autoincrement t)
    (name :text :not-null t)
    (color :text)
    (:table widgets))
  (schema-snapshot '(widget-v2)))

(defun %dir ()
  (let ((dir (make-script-directory))
        (v1 (%snap-v1))
        (v2 (%snap-v2)))
    (make-revision dir nil v1 :id "0001" :name "widgets")
    (make-revision dir v1 v2 :id "0002" :name "add-color")
    dir))

(deftest history-linear
  (let ((dir (%dir)))
    (ok (equal '("0001" "0002")
               (mapcar #'schema-migration-revision (history dir))))
    (ok (equal "0002" (head-revision dir)))))

(deftest unknown-revision-signals
  (ok (null (find-revision (make-script-directory) "nope")))
  (ok (signals (require-revision (make-script-directory) "nope")
               'unknown-revision)))

(deftest unknown-revision-use-value
  (let ((dir (%dir)))
    (ok (eq (find-revision dir "0001")
            (handler-bind ((unknown-revision
                            (lambda (c)
                              (use-value "0001" c))))
              (require-revision dir "missing"))))))

(deftest upgrade-head-and-downgrade-count
  (let ((dir (%dir)))
    (with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
      (ok (null (current-revision c dir)))
      (upgrade c dir)
      (ok (equal "0002" (current-revision c dir)))
      (sql-protocol:execute c "INSERT INTO widgets (name, color) VALUES (?, ?)"
                            '("w" "red"))
      (let ((row (sql-protocol:fetch
                  (sql-protocol:execute c "SELECT color FROM widgets"))))
        (ok (equal "red" (getf row :color))))
      (downgrade c dir :count 1)
      (ok (equal "0001" (current-revision c dir)))
      (ok (signals (sql-protocol:execute c "SELECT color FROM widgets") 'error))
      (downgrade c dir :target :base)
      (ok (null (current-revision c dir))))))

(deftest upgrade-count-then-noop
  (let ((dir (%dir)))
    (with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
      (upgrade c dir :count 1)
      (ok (equal "0001" (current-revision c dir)))
      (ok (sql-protocol:fetch
           (sql-protocol:execute c "SELECT name FROM sqlite_master WHERE name = 'widgets'")))
      (upgrade c dir :count 1)
      (ok (equal "0002" (current-revision c dir)))
      (ok (null (upgrade c dir)))
      (ok (null (downgrade c dir :target "0002"))))))

(deftest branch-not-supported
  (let ((dir (make-script-directory))
        (v1 (%snap-v1))
        (v2 (%snap-v2)))
    (register-revision dir (make-migration nil v1 :revision "a" :down-revision nil))
    (register-revision dir (make-migration v1 v2 :revision "b" :down-revision "a"))
    (register-revision dir (make-migration v1 v2 :revision "c" :down-revision "a"))
    (ok (signals (history dir) 'branch-not-supported))))

(deftest target-and-count-exclusive
  (let ((dir (%dir)))
    (with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
      (ok (signals (upgrade c dir :target :head :count 1) 'migrate-error))
      (ok (signals (downgrade c dir :target :base :count 1) 'migrate-error)))))

(deftest stamp-without-ops
  (let ((dir (%dir)))
    (with-orm-connection (c :driver :sqlite3 :database-name ":memory:")
      (stamp c dir "0002")
      (ok (equal "0002" (current-revision c dir)))
      (ok (null
           (sql-protocol:fetch
            (sql-protocol:execute c "SELECT name FROM sqlite_master WHERE name = 'widgets'")))))))

(in-package #:sql-migrate)

(defclass script-directory ()
  ((version-table :initarg :version-table :initform "sql_migrate_version"
                  :reader directory-version-table)
   (by-id :initform (make-hash-table :test #'equal) :reader directory-by-id)))

(defun make-script-directory (&key (version-table "sql_migrate_version"))
  (make-instance 'script-directory :version-table version-table))

(defun register-revision (dir migration)
  "Add a SCHEMA-MIGRATION. REVISION must be unique. Linear children only."
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
  (check-type dir script-directory)
  (gethash id (directory-by-id dir)))

(defun require-revision (dir id)
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
  "SCHEMA-MIGRATION list from root to head. Signals BRANCH-NOT-SUPPORTED if forked."
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
  "Revision id of the unique head, or NIL if empty."
  (let ((chain (history dir)))
    (when chain
      (sql-orm:schema-migration-revision (car (last chain))))))

(defun %next-id (dir)
  (format nil "~4,'0D" (1+ (hash-table-count (directory-by-id dir)))))

(defun make-revision (dir from-snapshot to-snapshot
                      &key id down-id name
                        (drop-tables nil) (drop-columns t))
  "Autogenerate a SCHEMA-MIGRATION from snapshots and register it.
   DOWN-ID defaults to the current head (NIL if empty)."
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

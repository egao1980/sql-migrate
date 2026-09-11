(in-package #:sql-migrate)

(defun %conn (connection)
  (sql-orm:orm-connection connection))

(defun %row-get (row key)
  (getf row (intern (string-upcase (string key)) :keyword)))

(defun %table (dir)
  (directory-version-table dir))

(defun ensure-version-table (connection dir)
  "CREATE TABLE IF NOT EXISTS the version table."
  (let ((conn (%conn connection))
        (table (%table dir)))
    (sql-protocol:execute
     conn
     (format nil "CREATE TABLE IF NOT EXISTS ~A (version_num TEXT NOT NULL PRIMARY KEY)"
             table))
    table))

(defun current-revision (connection dir)
  "Stamped revision id, or NIL at base."
  (ensure-version-table connection dir)
  (let* ((conn (%conn connection))
         (row (sql-protocol:fetch
               (sql-protocol:execute
                conn
                (format nil "SELECT version_num FROM ~A" (%table dir))))))
    (when row
      (or (%row-get row :version_num)
          (%row-get row :|VERSION_NUM|)))))

(defun stamp (connection dir revision)
  "Record REVISION (or NIL = base) without applying ops."
  (ensure-version-table connection dir)
  (let ((conn (%conn connection))
        (table (%table dir)))
    (sql-protocol:execute conn (format nil "DELETE FROM ~A" table))
    (when revision
      (require-revision dir revision)
      (sql-protocol:execute
       conn
       (format nil "INSERT INTO ~A (version_num) VALUES (?)" table)
       (list revision)))
    revision))

(defun %index (chain id)
  (if (null id)
      -1
      (or (position id chain
                    :key #'sql-orm:schema-migration-revision
                    :test #'equal)
          (error 'unknown-revision
                 :revision id
                 :message (format nil "revision ~S is not on this chain" id)))))

(defun %resolve-target (dir current target count direction)
  (let* ((chain (history dir))
         (cur-idx (%index chain current)))
    (cond
      ((and target count)
       (error 'migrate-error :message "specify :target or :count, not both"))
      (count
       (check-type count (integer 1))
       (let ((idx (ecase direction
                    (:up (min (1- (length chain)) (+ cur-idx count)))
                    (:down (max -1 (- cur-idx count))))))
         (if (minusp idx)
             nil
             (sql-orm:schema-migration-revision (nth idx chain)))))
      ((or (null target) (eq target :head))
       (head-revision dir))
      ((eq target :base) nil)
      ((stringp target)
       (require-revision dir target)
       (%index chain target) ; validate on chain
       target)
      (t
       (error 'migrate-error
              :revision target
              :message (format nil "bad target ~S" target))))))

(defun upgrade (connection dir &key target count)
  "Apply forward migrations from CURRENT to TARGET (:HEAD or a revision id).
   :COUNT N moves N revisions toward head. Default TARGET is :HEAD."
  (let* ((target (if (or target count) target :head))
         (current (current-revision connection dir))
         (dest (%resolve-target dir current target count :up))
         (chain (history dir))
         (from (%index chain current))
         (to (%index chain dest))
         (applied '()))
    (when (< to from)
      (error 'migrate-error
             :message "upgrade target is behind current; use downgrade"))
    (loop for i from (1+ from) to to
          for mig = (nth i chain)
          do (sql-orm:upgrade-schema connection mig)
             (stamp connection dir (sql-orm:schema-migration-revision mig))
             (push mig applied))
    (nreverse applied)))

(defun downgrade (connection dir &key target count)
  "Roll back toward TARGET (:BASE or a revision id). Default :COUNT 1 if TARGET omitted."
  (let* ((count (if (or target count) count 1))
         (current (current-revision connection dir))
         (dest (%resolve-target dir current (or target (and (null count) :base))
                                count :down))
         (chain (history dir))
         (from (%index chain current))
         (to (%index chain dest))
         (applied '()))
    (when (> to from)
      (error 'migrate-error
             :message "downgrade target is ahead of current; use upgrade"))
    (when (<= from to)
      (return-from downgrade nil))
    (loop for i from from downto (1+ to)
          for mig = (nth i chain)
          do (sql-orm:downgrade-schema connection mig)
             (stamp connection dir (sql-orm:schema-migration-down-revision mig))
             (push mig applied))
    (nreverse applied)))

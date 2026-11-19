(in-package #:sql-migrate)

;;; 0.1 runner API on the S2 layer: UPGRADE / DOWNGRADE resolve their goal
;;; along HISTORY (0.1 rules), then STACK-REV:PLAN-TRANSITION +
;;; STACK-REV:EXECUTE-PLAN on a SQL-TARGET (:CHECK-DRIFT NIL: 0.1 never
;;; checked drift). CURRENT-REVISION(S) and STAMP go through TARGET-REVISIONS
;;; / TARGET-STAMP.

(defun %table (dir)
  (directory-version-table dir))

(defun %target-of (connection dir &key (check-drift t))
  (make-sql-target connection :directory dir :check-drift check-drift))

(defun ensure-version-table (connection dir)
  "CREATE TABLE IF NOT EXISTS the version table."
  (%ensure-table connection (%table dir)))

(defun current-revisions (connection dir)
  "Every stamped revision id (version-table rows), sorted STRING<; NIL at
base. = (STACK-REV:TARGET-REVISIONS (MAKE-SQL-TARGET connection :DIRECTORY dir))."
  (rev:target-revisions (%target-of connection dir)))

(defun current-revision (connection dir)
  "Stamped revision id, or NIL at base. Several version-table rows →
MULTIPLE-HEADS (heads = the rows, sorted) with restart USE-VALUE."
  (let ((ids (current-revisions connection dir)))
    (if (rest ids)
        (restart-case
            (error 'multiple-heads
                   :heads ids
                   :message (format nil "the database is stamped with ~D revisions ~S"
                                    (length ids) ids))
          (use-value (id)
            :report "Use this revision id as the current one"
            id))
        (first ids))))

(defun stamp (connection dir revision)
  "Record REVISION (or NIL = base) without applying ops → the id stamped.
REVISION is checked first (REQUIRE-REVISION), so an unknown id signals
UNKNOWN-REVISION and leaves the version table as it was."
  (let ((id (and revision
                 (sql-orm:schema-migration-revision (require-revision dir revision)))))
    (rev:target-stamp (%target-of connection dir) (and id (list id)))
    id))

(defun %index (chain id)
  (if (null id)
      -1
      (or (position id chain
                    :key #'sql-orm:schema-migration-revision
                    :test #'equal)
          (error 'unknown-revision
                 :revision id
                 :designator id
                 :message (format nil "revision ~S is not on this chain" id)))))

(defun %resolve-target (dir current target count direction)
  "The goal id (NIL = base) for TARGET / COUNT from CURRENT (0.1 rules)."
  (let ((chain (history dir)))
    (cond
      ((and target count)
       (error 'migrate-error :message "specify :target or :count, not both"))
      (count
       (check-type count (integer 1))
       (let* ((cur-idx (%index chain current))
              (idx (ecase direction
                     (:up (min (1- (length chain)) (+ cur-idx count)))
                     (:down (max -1 (- cur-idx count))))))
         (if (minusp idx)
             nil
             (sql-orm:schema-migration-revision (nth idx chain)))))
      ((or (null target) (eq target :head))
       (head-revision dir))
      ((eq target :base) nil)
      ((stringp target)
       (let ((id (sql-orm:schema-migration-revision (require-revision dir target))))
         (%index chain id)              ; validate on chain
         id))
      (t
       (error 'migrate-error
              :revision target
              :message (format nil "bad target ~S" target))))))

(defun %check-current (dir current)
  "Signal UNKNOWN-REVISION when the stamp CURRENT is not in DIR."
  (when (and current (not (find-revision dir current)))
    (error 'unknown-revision
           :revision current
           :designator current
           :message (format nil "the database is stamped with ~S, which the directory does not hold"
                            current))))

(defun %migrate (connection dir dest direction)
  "Plan DIR's transition to DEST and run it → the migrations applied, in
application order (NIL when a handler chose ROLLBACK-TO-START)."
  (let* ((target (%target-of connection dir :check-drift nil))
         (plan (rev:plan-transition dir target dest)) ; always, even a no-op
         (wrong (if (eq direction :up) :down :up)))
    (when (find wrong (rev:plan-steps plan) :key #'rev:step-direction)
      (error 'migrate-error
             :message (if (eq direction :up)
                          "upgrade target is behind current; use downgrade"
                          "downgrade target is ahead of current; use upgrade")))
    ;; A failing step signals one condition that is a STEP-FAILED and of the
    ;; driver's class; it propagates with EXECUTE-PLAN's restarts in place.
    (let ((result (multiple-value-list (rev:execute-plan target plan))))
      (when (rest result)               ; completed: (revisions skipped)
        (let ((skipped (second result)))
          (loop for step in (rev:plan-steps plan)
                unless (member step skipped)
                  collect (find-revision dir (if (eq (rev:step-direction step) :up)
                                                 (rev:step-to step)
                                                 (rev:step-from step)))))))))

(defun upgrade (connection dir &key target count)
  "Apply forward migrations from CURRENT to TARGET (:HEAD or a revision id).
   :COUNT N moves N revisions toward head. Default TARGET is :HEAD.
   → the SCHEMA-MIGRATIONs applied, in order.
   PLAN-TRANSITION + EXECUTE-PLAN on a SQL-TARGET (:CHECK-DRIFT NIL). A
   failing step signals one condition that is both a STEP-FAILED and of the
   driver's condition class (0.1 handlers on the driver's class still fire)."
  (let* ((target (if (or target count) target :head))
         (current (current-revision connection dir))
         (dest (%resolve-target dir current target count :up)))
    (%check-current dir current)
    (%migrate connection dir dest :up)))

(defun downgrade (connection dir &key target count)
  "Roll back toward TARGET (:BASE or a revision id). Default :COUNT 1 if TARGET omitted.
   → the SCHEMA-MIGRATIONs rolled back, in rollback order. As UPGRADE."
  (let* ((count (if (or target count) count 1))
         (current (current-revision connection dir))
         (dest (%resolve-target dir current (or target (and (null count) :base))
                                count :down)))
    (%check-current dir current)
    (%migrate connection dir dest :down)))

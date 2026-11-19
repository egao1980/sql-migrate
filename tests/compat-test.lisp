(in-package #:sql-migrate/tests)

;;; S2 — the 0.1 API through the new layer: UPGRADE / DOWNGRADE are plans
;;; executed on a SQL-TARGET; CURRENT-REVISION(S) and STAMP read and write
;;; through it. Behaviour on linear graphs is 0.1's, plus the locked changes
;;; (STAMP checks first; MULTIPLE-HEADS for several version rows).

(deftest compat-upgrade-agrees-with-the-target
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (let ((applied (upgrade c dir))
            (target (make-sql-target c :directory dir)))
        (ok (equal '("0001" "0002") (mapcar #'schema-migration-revision applied)))
        (ok (eq (find-revision dir "0001") (first applied)) "the registered migrations")
        (ok (equal '("0002") (rev:target-revisions target)))
        (ok (null (rev:target-drift target dir)))
        (ok (equal '("0002") (current-revisions c dir)))
        (ok (equal '("0002" "0001")
                   (mapcar #'schema-migration-revision (downgrade c dir :target :base)))
            "rollback order")
        (ok (null (current-revisions c dir)))
        (ok (null (rev:target-state target)))))))

(deftest compat-upgrade-ignores-drift
  "0.1 never checked drift: tables outside the migrations do not block UPGRADE."
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (sql-protocol:execute c "CREATE TABLE app_extra (x INTEGER)")
      (ok (= 2 (length (upgrade c dir))))
      (ok (equal "0002" (current-revision c dir)))
      (ok (equal '("table:app_extra")
                 (diff:diff-footprint
                  (rev:target-drift (make-sql-target c :directory dir) dir)))))))

(deftest compat-current-revision-multiple-heads
  (let ((dir (linear-dir))
        (heads nil))
    (with-sqlite (c)
      (rev:target-stamp (make-sql-target c :directory dir) '("0001" "0002"))
      (ok (equal '("0001" "0002") (current-revisions c dir)))
      (ok (signals (current-revision c dir) 'multiple-heads))
      (ok (equal "0002"
                 (handler-bind ((multiple-heads
                                  (lambda (m)
                                    (setf heads (multiple-heads-heads m))
                                    (use-value "0002" m))))
                   (current-revision c dir))))
      (ok (equal '("0001" "0002") heads)))))

(deftest compat-stamp-checks-first
  "S2 (locked change): STAMP validates REVISION before touching the version table."
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (stamp c dir "0001")
      (ok (signals (stamp c dir "nope") 'unknown-revision))
      (ok (equal '("0001") (version-rows c dir)) "0.1 deleted the row first")
      (ok (null (stamp c dir nil)))
      (ok (null (current-revisions c dir))))))

(deftest compat-step-failure-resignals-cause
  "A failing step re-SIGNALs the driver's condition first (0.1 handlers fire),
then STEP-FAILED propagates to handlers for it."
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (%reject-stamp c dir "0002")
      (ok (eq :cause
              (handler-case (upgrade c dir)
                (rev:step-failed () :step-failed)
                (error (e) (if (typep e 'rev:revision-error) :other :cause)))))
      (ok (equal "0001" (current-revision c dir)))
      (ok (eq :step-failed
              (handler-case (upgrade c dir)
                (rev:step-failed () :step-failed))))
      (ok (eq :migrate
              (handler-case (upgrade c dir)
                (sqlite:sqlite-error () :migrate)))
          "the driver's own condition class, as in 0.1")
      (ok (not (%has-column-p c "widgets" "color"))))))

(deftest compat-step-failure-restarts-reach-the-caller
  "UPGRADE leaves EXECUTE-PLAN's restarts in place: RETRY-STEP from outside."
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (%reject-stamp c dir "0002")
      (ok (equal '("0001" "0002")
                 (mapcar #'schema-migration-revision
                         (handler-bind ((rev:step-failed
                                          (lambda (s)
                                            (declare (ignore s))
                                            (sql-protocol:execute c "DROP TRIGGER reject_stamp")
                                            (invoke-restart 'rev:retry-step))))
                           (upgrade c dir)))))
      (ok (equal "0002" (current-revision c dir))))))

(deftest compat-fork-refusals
  "On a fork: :HEAD → MULTIPLE-HEADS; an explicit id → BRANCH-NOT-SUPPORTED;
nothing is applied or stamped."
  (let ((dir (forked-dir)))
    (with-sqlite (c)
      (ok (signals (upgrade c dir) 'multiple-heads))
      (ok (handler-case (progn (upgrade c dir :target "b") nil)
            (multiple-heads () nil)
            (branch-not-supported () t)))
      (ok (signals (downgrade c dir :target :base) 'branch-not-supported))
      (ok (not (table-exists-p c "widgets")))
      (ok (null (current-revisions c dir))))))

(deftest compat-history-and-head-unchanged
  (let ((dir (linear-dir)))
    (ok (equal '("0001" "0002") (mapcar #'schema-migration-revision (history dir))))
    (ok (equal "0002" (head-revision dir)))
    (ok (null (history (make-script-directory))))
    (ok (null (head-revision (make-script-directory))))
    (ok (equal (mapcar #'rev:revision-id (rev:walk-revisions dir (head-revision dir)))
               (mapcar #'schema-migration-revision (history dir)))
        "history = the store's topological walk")))

(deftest compat-upgrade-unknown-current
  "A stamp the directory does not hold: UNKNOWN-REVISION, as 0.1."
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (rev:target-stamp (make-sql-target c :directory dir) '("zzzz"))
      (ok (signals (upgrade c dir) 'unknown-revision))
      (ok (signals (upgrade c dir) 'rev:revision-not-found))
      (ok (not (table-exists-p c "widgets"))))))

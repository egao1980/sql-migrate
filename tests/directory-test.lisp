(in-package #:sql-migrate/tests)

;;; S2 — SCRIPT-DIRECTORY as a revision-protocol store (:primary :delta),
;;; forks, and plans (against an in-memory target: no database).

(deftest directory-is-a-delta-store
  (let ((dir (linear-dir)))
    (ok (typep dir 'rev:in-memory-store))
    (ok (eq :delta (rev:content-primary dir)))
    (ok (typep (rev:content-engine dir) 'schema-diff-engine))
    (ok (equal "0002" (rev:revision-id (rev:resolve-revision dir "0002"))))
    (ok (equal '("0002") (mapcar #'rev:revision-id (rev:heads dir))))
    (ok (equal '("0001") (rev:revision-parent-ids (rev:resolve-revision dir "0002"))))
    (ok (null (rev:revision-parent-ids (rev:resolve-revision dir "0001"))))
    (ok (equal '("0001" "0002")
               (mapcar #'rev:revision-id (rev:walk-revisions dir "0002"))))))

(deftest directory-content-is-the-op-list
  "A revision's content is a SCHEMA-DIFF of its migration's upgrade ops."
  (let* ((dir (linear-dir))
         (r (rev:resolve-revision dir "0002"))
         (content (rev:revision-content r)))
    (ok (typep content 'schema-diff))
    (ok (eq content (rev:revision-delta dir "0002")) "a stored delta is returned as stored")
    (ok (equal (op-classes (migration-upgrade-ops (find-revision dir "0002")))
               (op-classes (schema-diff-ops content))))
    (ok (equal '("column:widgets.color") (rev:delta-footprint content)))
    (ok (equal '(create-table-op)
               (op-classes (schema-diff-ops (rev:revision-delta dir "0001")))))))

(deftest directory-revision-state-folds-ops
  (let ((dir (linear-dir))
        (v1 (%snap-v1))
        (v2 (%snap-v2)))
    (ok (schema-snapshot-equal v1 (rev:revision-state dir "0001")))
    (ok (schema-snapshot-equal v2 (rev:revision-state dir "0002")))
    (ok (rev:state-diff dir "0001" "0002"))
    (ok (equal '("column:widgets.color")
               (diff:diff-footprint (rev:state-diff dir "0001" "0002"))))
    (ok (diff:diff-empty-p (rev:state-diff dir "0002" "0002")))
    (ok (equal '("0002")
               (mapcar #'rev:revision-id
                       (rev:walk-revisions dir "0002" :footprint '("column:widgets.color"))))
        "walk :footprint finds the revision that touched the column")))

(deftest directory-make-revision-state
  "make-revision's to-snapshot is the folded state (0.1 autogenerate policy)."
  (let ((dir (make-script-directory))
        (v1 (%snap-v1))
        (v2 (%snap-v2)))
    (make-revision dir nil v1)
    (make-revision dir v1 v2)
    (ok (equal "0002" (head-revision dir)))
    (ok (schema-snapshot-equal v2 (rev:revision-state dir (head-revision dir))))
    ;; The autogenerate default keeps tables: v2 → nil drops nothing.
    (make-revision dir v2 nil)
    (ok (schema-snapshot-equal v2 (rev:revision-state dir "0003")))
    (ok (diff:diff-empty-p (rev:revision-delta dir "0003")))))

(deftest directory-register-unknown-parent
  "S2: a down-revision must already be registered (UNKNOWN-REVISION); nothing is stored."
  (let ((dir (linear-dir))
        (v2 (%snap-v2)))
    (ok (signals (register-revision dir (make-migration v2 nil :revision "x" :down-revision "nope"))
                 'unknown-revision))
    (ok (null (find-revision dir "x")))
    (ok (equal '("0002") (mapcar #'rev:revision-id (rev:heads dir))))
    (ok (signals (rev:resolve-revision dir "x") 'rev:revision-not-found))))

(deftest directory-duplicate-revision
  (let ((dir (linear-dir)))
    (ok (signals (register-revision dir (make-migration nil nil :revision "0001"))
                 'migrate-error))
    (ok (= 2 (length (history dir))))
    (ok (equal '("0002") (mapcar #'rev:revision-id (rev:heads dir))))))

(deftest directory-find-revision-is-exact
  "find-revision matches ids exactly; the store's resolve-revision accepts prefixes (R0a)."
  (let ((dir (make-script-directory)))
    (make-revision dir nil (%snap-v1) :id "widgets-0001")
    (ok (find-revision dir "widgets-0001"))
    (ok (null (find-revision dir "widg")))
    (ok (signals (require-revision dir "widg") 'unknown-revision))
    (ok (equal "widgets-0001" (rev:revision-id (rev:resolve-revision dir "widg"))))))

;;; ---------------------------------------------------------------------------
;;; Forks (S2 refuses them at planning; S3 plans them)
;;; ---------------------------------------------------------------------------

(deftest fork-history-is-topological
  (let* ((dir (forked-dir))
         (ids (mapcar #'schema-migration-revision (history dir))))
    (ok (= 3 (length ids)))
    (ok (equal "a" (first ids)) "parents before children")
    (ok (equal '("b" "c") (sort (rest ids) #'string<)))
    (ok (equal '("b" "c") (sort (mapcar #'rev:revision-id (rev:heads dir)) #'string<)))))

(deftest fork-head-revision-multiple-heads
  (let ((dir (forked-dir))
        (heads nil)
        (restarts nil))
    (ok (signals (head-revision dir) 'multiple-heads))
    (ok (equal "c"
               (handler-bind ((multiple-heads
                                (lambda (c)
                                  (setf heads (multiple-heads-heads c)
                                        restarts (restart-names c))
                                  (use-value "c" c))))
                 (head-revision dir))))
    (ok (equal '("b" "c") heads))
    (ok (member 'use-value restarts))))

(deftest fork-make-revision-needs-a-down-id
  "make-revision's default parent is HEAD-REVISION: on a fork it signals."
  (let ((dir (forked-dir)))
    (ok (signals (make-revision dir (%snap-v2) nil) 'multiple-heads))
    (ok (make-revision dir (%snap-v2) nil :id "d" :down-id "b"))
    (ok (equal '("a" "b" "d")
               (mapcar #'rev:revision-id (rev:walk-revisions dir "d"))))))

(deftest fork-plan-transition-refused
  "PLAN-TRANSITION on a forked directory signals exactly BRANCH-NOT-SUPPORTED,
before reading the target, whatever the goal."
  (let ((dir (forked-dir))
        (seen '()))
    (dolist (goal '("a" "b" nil))
      (ok (signals (rev:plan-transition dir (make-instance 'untouchable-target) goal)
                   'branch-not-supported))
      (ok (eq :refused
              (handler-case (progn (rev:plan-transition dir (memory-target "a") goal) :no-signal)
                (branch-not-supported (c) (push (type-of c) seen) :refused)))))
    (ok (equal '(branch-not-supported branch-not-supported branch-not-supported) seen)
        "not MULTIPLE-HEADS")))

;;; ---------------------------------------------------------------------------
;;; Plans on a linear directory
;;; ---------------------------------------------------------------------------

(defun %step-shape (step)
  (list (rev:step-direction step) (rev:step-from step) (rev:step-to step)))

(deftest plan-up-from-empty
  (let* ((dir (linear-dir))
         (plan (rev:plan-transition dir (memory-target) "0002"))
         (steps (rev:plan-steps plan)))
    (ok (eq dir (rev:plan-store plan)))
    (ok (null (rev:plan-start plan)))
    (ok (equal '("0002") (rev:plan-goal plan)))
    (ok (null (rev:plan-base plan)))
    (ok (equal '((:up nil "0001") (:up "0001" "0002")) (mapcar #'%step-shape steps)))
    (ok (eq (rev:revision-delta dir "0001") (rev:step-diff (first steps)))
        ":up step diff = the stored delta")
    (ok (eq (rev:revision-delta dir "0002") (rev:step-diff (second steps))))
    (ok (every #'rev:step-reversible-p steps))
    (ok (notany #'step-lossy-p steps))
    (ok (equal (format nil "up - -> 0001 table:widgets~%up 0001 -> 0002 column:widgets.color~%")
               (rev:describe-plan plan nil)))))

(deftest plan-down-to-empty
  (let* ((dir (linear-dir))
         (plan (rev:plan-transition dir (memory-target "0002") nil))
         (steps (rev:plan-steps plan)))
    (ok (equal '("0002") (rev:plan-start plan)))
    (ok (null (rev:plan-goal plan)) "NIL goal = the empty state")
    (ok (equal '((:down "0002" "0001") (:down "0001" nil)) (mapcar #'%step-shape steps)))
    (ok (equal '(drop-column-op) (op-classes (schema-diff-ops (rev:step-diff (first steps))))))
    (ok (equal '(drop-table-op) (op-classes (schema-diff-ops (rev:step-diff (second steps))))))
    (ok (every #'rev:step-reversible-p steps) "drops are lossy, not irreversible")
    (ok (every #'step-lossy-p steps))
    (ok (equal (format nil "down 0002 -> 0001 column:widgets.color~%down 0001 -> - table:widgets~%")
               (rev:describe-plan plan nil)))))

(deftest plan-partial-and-noop
  (let ((dir (linear-dir)))
    (ok (equal '((:up "0001" "0002"))
               (mapcar #'%step-shape
                       (rev:plan-steps (rev:plan-transition dir (memory-target "0001") "0002")))))
    (ok (equal '((:down "0002" "0001"))
               (mapcar #'%step-shape
                       (rev:plan-steps (rev:plan-transition dir (memory-target "0002") "0001")))))
    (let ((plan (rev:plan-transition dir (memory-target "0002") "0002")))
      (ok (null (rev:plan-steps plan)))
      (ok (equal "" (rev:describe-plan plan nil))))
    (ok (signals (rev:plan-transition dir (memory-target) "zzzz") 'rev:revision-not-found))
    (ok (signals (rev:plan-transition dir (memory-target "0001" "0002") "0002")
                 'rev:revision-error)
        "several stamps: multi-head plans are S3")))

(deftest plan-is-pure
  (let* ((dir (linear-dir))
         (target (memory-target "0001")))
    (rev:plan-transition dir target "0002")
    (ok (equal '("0001") (rev:target-revisions target)))
    (ok (equal '("0002") (mapcar #'rev:revision-id (rev:heads dir))))))

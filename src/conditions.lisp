(in-package #:sql-migrate)

;;; Condition tree (S2 — brief "sql-migrate 0.2.0 — S2 locked"):
;;;
;;;   stack-rev:revision-error (message)
;;;     migrate-error (revision)
;;;       unknown-revision           also stack-rev:revision-not-found
;;;                                  (designator = the id)   restart: use-value
;;;       branch-not-supported       deprecated; PLAN-TRANSITION on a forked
;;;                                  directory signals it (no longer HISTORY)
;;;         multiple-heads (heads)   HEAD-REVISION / CURRENT-REVISION with
;;;                                  several heads           restart: use-value
;;;
;;; A failing plan step signals one condition that is both a
;;; STACK-REV:STEP-FAILED and an instance of the driver's condition class
;;; (see %STEP-FAILED-CLASS in target.lisp).

(define-condition migrate-error (rev:revision-error)
  ((revision :initarg :revision :reader migrate-error-revision :initform nil))
  (:report (lambda (c s)
             (format s "sql-migrate error~@[: ~A~]" (rev:revision-error-message c))))
  (:documentation "Root of sql-migrate's conditions, a subtype of
STACK-REV:REVISION-ERROR. Its message is the revision-error message:
MIGRATE-ERROR-MESSAGE and STACK-REV:REVISION-ERROR-MESSAGE return the same
string. REVISION is the id concerned, or NIL."))

(defun migrate-error-message (condition)
  "The message of CONDITION, a MIGRATE-ERROR (= STACK-REV:REVISION-ERROR-MESSAGE)."
  (check-type condition migrate-error)
  (rev:revision-error-message condition))

(define-condition unknown-revision (migrate-error rev:revision-not-found) ()
  (:report (lambda (c s)
             (format s "sql-migrate: unknown revision ~S~@[: ~A~]"
                     (rev:revision-not-found-designator c)
                     (rev:revision-error-message c))))
  (:documentation "A revision id the directory does not hold. Also a
STACK-REV:REVISION-NOT-FOUND whose designator is the id."))

(define-condition branch-not-supported (migrate-error) ()
  (:documentation "Deprecated (S3 adds branches). Signalled by
PLAN-TRANSITION (and so UPGRADE / DOWNGRADE) on a directory with more than one
head; HISTORY no longer signals it. Parent of MULTIPLE-HEADS."))

(define-condition multiple-heads (branch-not-supported)
  ((heads :initarg :heads :reader multiple-heads-heads :initform nil))
  (:documentation "One revision was asked for and there are several: HEADS
(ids, sorted STRING<). Signalled by HEAD-REVISION (directory heads) and
CURRENT-REVISION (version-table rows), each with a USE-VALUE restart."))

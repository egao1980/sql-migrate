(in-package #:sql-migrate)

;;; Condition tree (S2 target — brief "sql-migrate 0.2.0 — S2 locked"):
;;;
;;;   stack-rev:revision-error
;;;     migrate-error (message revision)
;;;       unknown-revision           also stack-rev:revision-not-found
;;;                                  (designator = the id)   restart: use-value
;;;       branch-not-supported       deprecated; PLAN-TRANSITION on a forked
;;;                                  directory signals it (no longer HISTORY)
;;;         multiple-heads (heads)   HEAD-REVISION / CURRENT-REVISION with
;;;                                  several heads           restart: use-value
;;;
;;; CONTRACT: the 0.1 tree is kept below (MIGRATE-ERROR is a plain ERROR) so
;;; the 0.1 suite keeps running; the S2 implementation re-parents it.

(define-condition migrate-error (error)
  ((message :initarg :message :reader migrate-error-message :initform nil)
   (revision :initarg :revision :reader migrate-error-revision :initform nil))
  (:report (lambda (c s)
             (format s "sql-migrate error~@[: ~A~]" (migrate-error-message c))))
  (:documentation "Root of sql-migrate's conditions. S2: a subtype of
STACK-REV:REVISION-ERROR; MIGRATE-ERROR-MESSAGE and
STACK-REV:REVISION-ERROR-MESSAGE return the same string."))

(define-condition unknown-revision (migrate-error) ()
  (:documentation "A revision id the directory does not hold. S2: also a
STACK-REV:REVISION-NOT-FOUND whose designator is the id."))

(define-condition branch-not-supported (migrate-error) ()
  (:documentation "Deprecated (S3 adds branches). S2: signalled by
PLAN-TRANSITION (and so UPGRADE / DOWNGRADE) on a directory with more than one
head; HISTORY no longer signals it. Parent of MULTIPLE-HEADS."))

(define-condition multiple-heads (branch-not-supported)
  ((heads :initarg :heads :reader multiple-heads-heads :initform nil))
  (:documentation "One revision was asked for and there are several: HEADS
(ids, sorted STRING<). Signalled by HEAD-REVISION (directory heads) and
CURRENT-REVISION (version-table rows), each with a USE-VALUE restart."))

;;; Contract stubs signal this; it is deliberately not a MIGRATE-ERROR or a
;;; REVISION-ERROR, so no test can pass on a stub by expecting one of those.
(define-condition not-implemented (error)
  ((what :initarg :what :reader not-implemented-what))
  (:report (lambda (c s)
             (format s "~A is not implemented yet (sql-migrate 0.2.0 S2)"
                     (not-implemented-what c)))))

(defun %not-implemented (what)
  (error 'not-implemented :what what))

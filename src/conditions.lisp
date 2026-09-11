(in-package #:sql-migrate)

(define-condition migrate-error (error)
  ((message :initarg :message :reader migrate-error-message :initform nil)
   (revision :initarg :revision :reader migrate-error-revision :initform nil))
  (:report (lambda (c s)
             (format s "sql-migrate error~@[: ~A~]" (migrate-error-message c)))))

(define-condition unknown-revision (migrate-error) ())
(define-condition branch-not-supported (migrate-error) ())

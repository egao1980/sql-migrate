(in-package #:sql-migrate/tests)

;;; S2 — condition tree: handlers written for revision-protocol conditions
;;; catch sql-migrate's. Handlers sit inside OK; restarts are recorded inside
;;; the handler.

(deftest condition-tree
  (ok (subtypep 'migrate-error 'rev:revision-error))
  (ok (subtypep 'unknown-revision 'rev:revision-not-found))
  (ok (subtypep 'unknown-revision 'migrate-error))
  (ok (subtypep 'branch-not-supported 'migrate-error))
  (ok (subtypep 'multiple-heads 'branch-not-supported))
  (ok (not (subtypep 'multiple-heads 'rev:revision-not-found))))

(deftest condition-messages-agree
  (let ((c (make-condition 'migrate-error :message "m" :revision "r")))
    (ok (equal "m" (migrate-error-message c)))
    (ok (equal "m" (rev:revision-error-message c)))
    (ok (equal "r" (migrate-error-revision c)))))

(deftest condition-revision-error-handler-catches-duplicate
  (let ((dir (linear-dir)))
    (ok (eq :caught
            (handler-case (register-revision dir (make-migration nil nil :revision "0001"))
              (rev:revision-error () :caught))))))

(deftest condition-not-found-handler-catches-unknown
  (let ((dir (linear-dir))
        (seen nil))
    (ok (eq (find-revision dir "0001")
            (handler-bind ((rev:revision-not-found
                             (lambda (c)
                               (setf seen (list (rev:revision-not-found-designator c)
                                                (migrate-error-revision c)
                                                (restart-names c)))
                               (use-value "0001" c))))
              (require-revision dir "missing"))))
    (ok (equal "missing" (first seen)) "designator = the id")
    (ok (equal "missing" (second seen)))
    (ok (member 'use-value (third seen)))))

(deftest condition-not-found-handler-on-upgrade-target
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (ok (eq :not-found
              (handler-case (upgrade c dir :target "nope")
                (rev:revision-not-found () :not-found)))))))

(deftest condition-revision-error-handler-catches-fork-refusals
  (let ((dir (forked-dir)))
    (ok (eq :refused
            (handler-case (head-revision dir)
              (rev:revision-error () :refused))))
    (ok (eq :refused
            (handler-case (rev:plan-transition dir (memory-target) "b")
              (rev:revision-error () :refused))))))

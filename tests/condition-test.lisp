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

;;; Issue #5 — reports say each thing once.

(defun %occurrences (needle haystack)
  (loop with n = 0
        for start = (search needle haystack) then (search needle haystack :start2 (1+ start))
        while start
        do (incf n)
        finally (return n)))

(deftest condition-combined-report-prints-driver-block-once
  "The combined STEP-FAILED/<driver> condition prints the cause once: cl-sqlite's
PRINT-OBJECT :AFTER block (Code / Database / SQL) must not run again on the
combined condition."
  (let ((dir (linear-dir)))
    (with-sqlite (c)
      (%reject-stamp c dir "0002")
      (let* ((e (handler-case (upgrade c dir) (error (e) e)))
             (text (princ-to-string e)))
        (ok (typep e 'rev:step-failed))
        (ok (search "plan step" text) "the STEP-FAILED report")
        (ok (= 1 (%occurrences "SQL: INSERT" text)) "the SQL line once")
        (ok (= 1 (%occurrences "Code CONSTRAINT" text)) "the Code line once")
        (ok (search "#<" (prin1-to-string e)) "PRIN1 is unaffected")))))

(deftest condition-unknown-revision-names-designator-once
  (let ((dir (linear-dir)))
    (let ((text (princ-to-string
                 (handler-case (require-revision dir "nope")
                   (unknown-revision (e) e)))))
      (ok (= 1 (%occurrences "nope" text)) text)
      (ok (search "unknown revision" text)))
    (let ((text (princ-to-string (make-condition 'unknown-revision :designator "zz"))))
      (ok (= 1 (%occurrences "zz" text)) "no message: the designator alone"))))

;;; Issue #4 — the combined condition: slot copy, class cache, initargs.

(define-condition %driver-error (error)
  ((detail :initarg :detail :reader %driver-error-detail)
   (step :initarg :step :reader %driver-error-step)
   (unset :initarg :unset :reader %driver-error-unset))
  (:report (lambda (c s) (format s "driver failed: ~A" (%driver-error-detail c)))))

(deftest condition-combined-carries-driver-slots
  (let* ((cause (make-condition '%driver-error :detail "boom" :step :driver-step))
         (e (handler-case (sql-migrate::%signal-step-failed :the-step cause 2)
              (error (e) e))))
    (ok (typep e 'rev:step-failed))
    (ok (typep e '%driver-error))
    (ok (eq cause (rev:step-failed-cause e)))
    (ok (equal "boom" (%driver-error-detail e)) "driver slot copied")
    (ok (= 2 (rev:step-failed-attempt e)))
    (ok (eq :the-step (rev:step-failed-step e)) "a shared initarg: STEP-FAILED's value wins")
    (ok (eq :the-step (%driver-error-step e)))
    (ok (not (slot-boundp e 'unset)) "unbound driver slots stay unbound")
    (ok (= 1 (%occurrences "boom" (princ-to-string e))))))

(deftest condition-combined-class-built-once-across-threads
  (define-condition %fresh-driver-error (error) ())
  (let* ((cause (make-condition '%fresh-driver-error))
         (results (make-array 8 :initial-element nil))
         (threads (loop for i below 8
                        collect (let ((i i))
                                  (bt:make-thread
                                   (lambda ()
                                     (setf (aref results i)
                                           (sql-migrate::%step-failed-class cause))))))))
    (mapc #'bt:join-thread threads)
    (ok (every #'symbolp results))
    (ok (= 1 (length (remove-duplicates (coerce results 'list)))) "one class for all threads")))

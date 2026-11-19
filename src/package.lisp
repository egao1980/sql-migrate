(defpackage #:sql-migrate
  (:use #:cl)
  (:nicknames #:stack-sql-migrate)
  (:local-nicknames (#:rev #:revision-protocol)
                    (#:diff #:diff-protocol))
  (:export
   ;; conditions (0.1; S2: MIGRATE-ERROR is a STACK-REV:REVISION-ERROR,
   ;; UNKNOWN-REVISION also a STACK-REV:REVISION-NOT-FOUND)
   #:migrate-error
   #:unknown-revision
   #:branch-not-supported            ; deprecated: parent of the S2 fork refusals
   #:migrate-error-message
   #:migrate-error-revision
   ;; conditions (S2)
   #:multiple-heads
   #:multiple-heads-heads

   ;; script directory (0.1; S2: a revision-protocol store, :primary :delta)
   #:script-directory
   #:make-script-directory
   #:directory-version-table
   #:register-revision
   #:find-revision
   #:require-revision
   #:head-revision
   #:history
   #:make-revision

   ;; schema diff engine (S2, a diff-protocol engine over sql-orm snapshots)
   #:schema-diff-engine
   #:make-schema-diff-engine
   #:schema-engine-dialect
   #:schema-diff
   #:make-schema-diff
   #:schema-diff-ops
   #:schema-change
   #:change-schema-op
   #:change-lossy-p
   #:diff-lossy-p
   #:step-lossy-p

   ;; SQL target (S2, a revision-protocol target)
   #:sql-target
   #:make-sql-target
   #:target-connection
   #:target-directory
   #:target-version-table
   #:target-dialect
   #:target-check-drift-p

   ;; runner (0.1)
   #:ensure-version-table
   #:current-revision
   #:current-revisions               ; S2
   #:stamp
   #:upgrade
   #:downgrade))

(in-package #:sql-migrate)

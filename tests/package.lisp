(defpackage #:sql-migrate/tests
  (:use #:cl #:rove #:sql-migrate #:sql-orm)
  (:local-nicknames (#:rev #:revision-protocol)
                    (#:diff #:diff-protocol)))

(in-package #:sql-migrate/tests)

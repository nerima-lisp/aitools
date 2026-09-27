;;;; Fixture: dependencies declared in defpackage clauses, not only in code.
(in-package #:cl-user)
(defpackage #:aitools.fixture.domain
  (:use #:cl #:cl-cli)
  (:import-from #:aitools.kernel.domain #:sha256-hex)
  (:import-from #:process-kit #:run)
  (:shadowing-import-from #:vcs-kit #:status)
  (:local-nicknames (#:hk #:host-kit)))

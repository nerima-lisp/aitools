;;;; packages/core/store/src/infrastructure/package.lisp
;;;;
;;;; The production STORE-IO adapter: sb-posix for file primitives and fsync,
;;;; libc flock(2) through sb-alien (sb-posix binds lockf and fcntl record
;;;; locks but not flock, which the state locks use), and cl-boundary-kit's clock,
;;;; sleeper and environment boundaries.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(in-package #:cl-user)

(defpackage #:aitools.store.infrastructure
  (:use #:cl)
  (:import-from #:aitools.store.domain #:octets #:state-home)
  (:import-from #:aitools.store.application #:make-store-io #:store-io-error #:make-store)
  (:export
   #:make-posix-store-io
   #:make-posix-store))

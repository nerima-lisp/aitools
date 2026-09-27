;;;; packages/feature/search/src/application/package.lisp
;;;;
;;;; The search context's use cases: `search`, `find`,
;;;; `code outline|defs|refs`, and `overview`. Each flow follows the command
;;;; handler contract: it calls exactly one of ON-OK, ON-PARTIAL, ON-ERROR
;;;; with an envelope-ordered alist (or an error code, message, and repairs)
;;;; and never writes output or picks an exit code itself.
(in-package #:cl-user)

(defpackage #:aitools.search.application
  (:use #:cl #:aitools.search.domain)
  (:import-from #:aitools.protocol.domain #:json-object-from-alist #:json-null #:json-boolean)
  (:export
   ;; ports.lisp
   #:search-ports
   #:search-ports-p
   #:make-search-ports
   ;; search-flow.lisp
   #:search/k
   ;; find-flow.lisp
   #:find/k
   ;; code-flow.lisp
   #:code-outline/k
   #:code-defs/k
   #:code-refs/k
   ;; overview-flow.lisp
   #:overview/k))

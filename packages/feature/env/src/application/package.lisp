;;;;
;;;; Environment and time flows receive a boundary context and return through
;;;; named command-result continuations.
(in-package #:cl-user)

(defpackage #:aitools.env.application
  (:use #:cl)
  (:import-from #:aitools.protocol.domain #:repair)
  (:export
   #:local-zone-name
   #:resolve-zone/k
   #:time-now/k
   #:time-convert/k
   #:time-diff/k
   #:find-executable
   #:sys-info/k
   #:sys-snapshot/k
   #:sys-env/k
   #:sys-tools/k
   #:sys-procs/k
   #:sys-metrics/k
   #:sys-network/k
   #:sys-disk/k
   #:sys-ports/k))

;;;; packages/feature/env/src/domain/package.lisp
;;;;
;;;; Pure rules of the env context: calendar and ISO 8601 arithmetic, time
;;;; input detection, the TZif reader and POSIX TZ rules, and parsers for the
;;;; OS text sources behind `sys`. No I/O.
(in-package #:cl-user)

(defpackage #:aitools.env.domain
  (:use #:cl)
  ;; days-from-civil is the shared calendar primitive homed in text.domain
  ;; (codec-zip needs the same conversion); env's civil-time and posix-tz
  ;; call it unqualified.
  (:import-from #:aitools.text.domain #:days-from-civil)
  (:import-from #:aitools.protocol.domain #:json-null)
  (:export
   ;; civil-time.lisp
   #:ascii-digit-value
   #:ascii-digits-p
   #:leap-year-p
   #:days-in-month
   ;; days-from-civil is imported from text.domain and re-exported here.
   #:days-from-civil
   #:civil-from-days
   #:civil-to-epoch-milliseconds
   #:epoch-milliseconds-to-civil
   #:epoch-milliseconds-in-range-p
   #:format-utc-offset
   #:format-iso8601
   #:format-human-duration
   ;; time-input.lisp
   #:time-syntax-error
   #:time-syntax-error-text
   #:time-syntax-error-reason
   #:time-input
   #:time-input-kind
   #:time-input-format
   #:time-input-milliseconds
   #:parse-time-input
   ;; posix-tz.lisp
   #:posix-tz-syntax-error
   #:posix-tz
   #:parse-posix-tz
   #:posix-tz-offset-at
   #:posix-tz-standard-name
   #:posix-tz-daylight-name
   ;; tzif.lisp
   #:tzif-format-error
   #:tzif-format-error-reason
   #:zone
   #:zone-name
   #:zone-footer
   #:zone-transitions
   #:make-fixed-zone
   #:parse-tzif
   #:zone-offset-at
   #:zone-local-to-epoch-milliseconds
   ;; host-parsers.lisp
   #:split-fields
   #:parse-meminfo
   #:parse-vm-stat
   #:parse-sysctl-values
   #:count-cpuinfo-processors
   #:parse-etime
   #:parse-ps-output
   #:parse-proc-stat-process
   #:parse-proc-boot-time
   #:parse-proc-status-uid
   #:parse-passwd
   #:proc-cmdline-command
   #:process-matches-pattern-p
   #:format-ipv6
   #:parse-proc-net-tcp
   #:parse-socket-inode
   #:parse-lsof-listen
   #:sort-and-deduplicate-ports
   ;; host-values.lisp
   #:json-null
   #:secret-environment-name-p
   #:split-search-path
   #:join-directory
   #:first-output-line
   #:valid-tool-name-p
   #:valid-zone-name-p
   #:utc-zone-name-p
   #:zoneinfo-directories
   #:zone-name-from-tz-variable
   #:zone-name-from-localtime-link
   #:octets-from-latin-1))

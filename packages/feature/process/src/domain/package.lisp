;;;; packages/feature/process/src/domain/package.lisp
;;;;
;;;; Pure rules for `run`, `wait`, and `bg`: terminal-output
;;;; normalization (ANSI escapes, `\r` progress lines), head/tail truncation,
;;;; `--grep` over the full output, the bg record format and its identifiers,
;;;; log slicing by byte offset, and the `wait` condition model. Nothing here
;;;; starts a process or touches the filesystem.
(in-package #:cl-user)

(defpackage #:aitools.process.domain
  (:use #:cl)
  (:import-from #:aitools.protocol.domain #:json-object #:json-object-from-alist #:json-boolean)
  (:import-from #:aitools.text.domain #:decode-utf8)
  (:export
   ;; json-values.lisp
   #:json-or-null
   ;; shell-words.lisp
   #:command-line
   ;; line-pattern.lisp
   #:invalid-line-pattern
   #:invalid-line-pattern-message
   #:compile-line-pattern
   #:line-pattern-matches-p
   ;; terminal-text.lisp
   #:strip-ansi-escapes
   #:collapse-carriage-returns
   #:normalize-terminal-line
   #:split-output-lines
   #:decode-output-octets
   ;; output-report.lisp
   #:output-report
   #:output-report-p
   #:output-report-head
   #:output-report-tail
   #:output-report-total-lines
   #:output-report-truncated
   #:output-report-matches
   #:output-report-total-matches
   #:output-report-redactions
   #:output-report-grep-exceeded-p
   #:summarize-output
   #:output-report-json
   ;; process-outcome.lisp
   #:process-outcome
   #:make-process-outcome
   #:process-outcome-p
   #:process-outcome-exit-code
   #:process-outcome-signal
   #:process-outcome-timed-out
   #:process-outcome-duration-ms
   #:process-outcome-stdout
   #:process-outcome-stderr
   #:process-outcome-stdout-capped
   #:process-outcome-stderr-capped
   #:process-outcome-stdout-bytes
   #:run-result-fields
   ;; bg-record.lisp
   #:bg-record
   #:make-bg-record
   #:bg-record-p
   #:bg-record-id
   #:bg-record-name
   #:bg-record-argv
   #:bg-record-pid
   #:bg-record-started
   #:bg-record-stop-signal
   #:copy-bg-record-with-stop-signal
   #:invalid-bg-record
   #:invalid-bg-record-message
   #:bg-id-p
   #:bg-id-number
   #:bg-name-valid-p
   #:next-bg-id
   #:bg-record-file-name
   #:bg-log-file-name
   #:bg-exit-file-name
   #:bg-record-id-from-file-name
   #:bg-file-id
   #:serialize-bg-record
   #:parse-bg-record
   #:parse-exit-status
   #:format-utc-timestamp
   #:bg-status-item
   ;; log-slice.lisp
   #:log-slice
   #:log-slice-p
   #:log-slice-lines
   #:log-slice-next-offset
   #:log-slice-truncated
   #:log-slice-redactions
   #:slice-log
   ;; wait-condition.lisp
   #:wait-condition
   #:wait-condition-p
   #:wait-condition-kind
   #:wait-condition-path
   #:wait-condition-pattern
   #:wait-condition-port
   #:wait-condition-bg-id
   #:wait-condition-duration-text
   #:wait-condition-duration-ms
   #:make-wait-condition
   #:wait-condition-arguments
   #:first-matching-line))

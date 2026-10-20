(in-package #:aitools.search.application)

(defparameter *search-regex-budget-seconds* 20
  "Wall-clock seconds a single `search` run may spend matching before it gives
up on the remaining advanced-executor work and reports the file skipped
`regex-limit`. Generous enough that a linear scan of a large tree never
reaches it; tests rebind it small.")

(defun %run-deadline ()
  (and *search-regex-budget-seconds*
       (+ (get-internal-real-time)
          (* *search-regex-budget-seconds* internal-time-units-per-second))))

(defun %search-worker (session matcher mode keep deadline)
  "The per-file WORK function: a FILE-OUTCOME, or (:SKIPPED reason). DEADLINE
(an internal-real-time or NIL) is the run's cumulative regex budget, bound on
this worker thread so MAP-MATCHES can observe it."
  (lambda (entry)
    (when (eq (aitools.workspace.application:scan-entry-kind entry) :file)
      (handler-case
          (aitools.kernel.domain:flow-call
              (call-with-regex-budget/k
               (lambda ()
                 (let ((*regex-run-deadline* deadline))
                   (aitools.kernel.domain:flow-call
                       (%read-file/k
                        session
                        (aitools.workspace.application:scan-entry-absolute entry)
                        (aitools.workspace.application:scan-entry-path entry))
                     (:on-text (octets)
                       (search-file matcher octets mode keep))
                     (:on-binary ()
                       (list :skipped "binary"))
                     (:on-missing ()
                       (list :skipped "unreadable"))))))
            (:on-result (result) result)
            (:on-exhausted ()
              (list :skipped "regex-limit")))
        ((or stream-error file-error) ()
          (list :skipped "unreadable"))))))

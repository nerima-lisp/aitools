;;;; packages/feature/edit/src/application/pipeline.lisp
;;;;
;;;; RUN-WRITE-COMMAND/K: the path every workspace-changing command takes
;;;; (guards, input, output, redaction refusal, the workspace boundary,
;;;; the store commit, and tx staging; see
;;;; docs/src/reference/commands.md and transactions.md). A command
;;;; describes its write as a WRITE-PLAN (write-plan.lisp: targets, inputs,
;;;; guards, and a PLAN function that turns the current state into change
;;;; requests); this file resolves the root, refuses redacted input and
;;;; missing guards, checks the boundary before and again after taking the
;;;; lock (the checks themselves are in write-guards.lisp), compares
;;;; --expect-hash against the state the write sees (the disk, or the tx
;;;; overlay), and hands the requests to the store's commit, its tx
;;;; staging, or a dry-run plan. It renders the write output with the
;;;; store's WRITE-RESULT-FIELDS, so every write command answers alike.
(in-package #:aitools.edit.application)

(defun %extend-change (change per-change)
  (let* ((members (aitools.protocol.domain:json-object-members change))
         (more
          (cdr
           (assoc (cdr (assoc "path" members :test #'string=))
                  per-change
                  :test
                  #'equal))))
    (if more
        (aitools.protocol.domain:json-object-from-alist (append members more))
        change)))

(defun %record-options (write-plan paths)
  (let ((options (write-plan-record-options write-plan)))
    (if (functionp options)
        (funcall options paths)
        options)))

(defun %with-extra-fields (fields extra)
  "FIELDS (from WRITE-RESULT-FIELDS) with the plan's EXTRA string-keyed
fields inserted before `next_commands`. Two keyword entries adjust
`changes` itself: (:PER-CHANGE . ((path . members) ...)) appends members to
that path's change, (:LIMIT-CHANGES . N) keeps the first N changes, and
(:TOTAL-CHANGES . T) adds `total`, the count before that limit."
  (let* ((per-change (cdr (assoc :per-change extra)))
         (limit (cdr (assoc :limit-changes extra)))
         (total
          (and (assoc :total-changes extra)
               (list
                (cons "total"
                      (length (cdr (assoc "changes" fields :test #'string=)))))))
         (plain
          (append
           (remove-if
            (lambda (pair)
              (keywordp (car pair)))
            extra)
           total))
         (fields
          (mapcar
           (lambda (pair)
             (if (string= (car pair) "changes")
                 (let ((changes
                        (mapcar
                         (lambda (change)
                           (%extend-change change per-change))
                         (cdr pair))))
                   (cons "changes"
                         (if limit
                             (subseq changes 0 (min limit (length changes)))
                             changes)))
                 pair))
           fields))
         (next (assoc "next_commands" fields :test #'string=)))
    (append (remove next fields) plain (and next (list next)))))

(aitools.kernel.domain:define-flow
 (run-plan context plan &key on-committed on-rejected)
 (declare (type function plan))
 (handler-case (call-with-regex-refusals
                (lambda ()
                  (funcall plan
                           context
                           :on-committed
                           (lambda (requests &optional extra)
                             (aitools.kernel.domain:flow-continue :on-committed
                                                                  requests
                                                                  extra))
                           :on-rejected
                           (lambda
                               (code message &key candidates diagnostics conflicts repairs path)
                             (aitools.kernel.domain:flow-continue :on-rejected code message
                                                  :candidates candidates
                                                  :diagnostics diagnostics
                                                  :conflicts conflicts
                                                  :repairs repairs
                                                  :path path)))))
   (aitools.edit.domain:edit-refusal (condition)
     (aitools.kernel.domain:flow-continue :on-rejected
                                          (aitools.edit.domain:edit-refusal-code
                                           condition)
                                          (aitools.edit.domain:edit-refusal-detail
                                           condition)))
   (aitools.text.domain:archive-unsupported (condition)
     (aitools.kernel.domain:flow-continue :on-rejected
                                          "input.unsupported-format"
                                          (princ-to-string condition)))
   (aitools.text.domain:archive-error (condition)
     (aitools.kernel.domain:flow-continue :on-rejected
                                          "input.syntax-error"
                                          (princ-to-string condition)))
   (aitools.store.application:store-io-error (condition)
     (aitools.kernel.domain:flow-continue :on-rejected
                                          "environment.io"
                                          (princ-to-string condition)))))

(defun %write-plan-guard-failure (write-plan hash-entries expect-count dry-run argv display-path)
  (loop with failure = nil
        for requirement in (write-plan-guard-requirements write-plan)
        until failure
        do (setf failure
                 (ecase (first requirement)
                   (:expect-hash
                    (let ((path (second requirement)))
                      (unless
                          (find-if
                           (lambda (entry)
                             (let ((entry-path
                                    (aitools.kernel.domain:expect-hash-entry-path
                                     entry)))
                               (or (null path)
                                   (equal path (or entry-path display-path)))))
                           hash-entries)
                        (list "argument.invalid"
                              (format nil
                                      "this write needs --expect-hash ~:[<hash>~;~:*~A=<hash>~]"
                                      (and
                                       (rest (write-plan-targets write-plan))
                                       path))
                              (list
                               (repair "get-hash"
                                       "Read the current hash, then pass that as --expect-hash."
                                       (format nil
                                               "aitools info ~A"
                                               (aitools.protocol.domain:shell-quote
                                                (or path display-path))))))))) (:expect-count
                    (unless (or expect-count dry-run)
                      (list "argument.invalid"
                            "this write needs --expect-count N"
                            (list
                             (repair "count"
                                     "Count the selection without writing, then pass that --expect-count."
                                     (dry-run-command-line argv))))))))
        finally (return failure)))

(aitools.kernel.domain:define-flow
 (%inspect-write-plan/k write-plan
                        lock-timeout
                        dry-run
                        argv
                        display-path
                        &key
                        on-ok
                        on-error)
 "Inspect WRITE-PLAN without reading workspace state and call one continuation."
 (multiple-value-bind (lock-timeout-ms lock-timeout-valid)
     (%lock-timeout-ms lock-timeout)
   (multiple-value-bind (hash-entries hash-error)
       (%parse-expect-hashes (write-plan-expect-hashes write-plan))
     (let* ((expect-count-text (write-plan-expect-count write-plan))
            (expect-count
             (and expect-count-text (parse-count expect-count-text)))
            (failure
             (cond
              ((not lock-timeout-valid)
               (list "argument.invalid"
                     (format nil
                             "--lock-timeout ~S is not a duration (<n>ms|s|m|h|d)"
                             lock-timeout)
                     nil))
              (hash-error (list "argument.invalid" hash-error nil))
              ((and expect-count-text (null expect-count))
               (list "argument.invalid"
                     (format nil
                             "--expect-count ~S is not a non-negative integer"
                             expect-count-text)
                     nil)))))
       (unless failure
         (setf failure
               (%write-plan-guard-failure write-plan
                                           hash-entries
                                           expect-count
                                           dry-run
                                           argv
                                           display-path)))
       (unless failure
         (let ((redacted
                (find-if #'%contains-placeholder-p
                         (write-plan-inputs write-plan))))
           (when redacted
             (setf failure
                   (list "refusal.redacted-input"
                         (format nil
                                 "the input contains ~A, an output mask rather than real content"
                                 +redaction-placeholder+)
                         nil)))))
       (if failure
           (aitools.kernel.domain:flow-continue
            :on-error
            (first failure)
            (second failure)
            (third failure))
           (aitools.kernel.domain:flow-continue
            :on-ok
            lock-timeout-ms
            hash-entries
            expect-count))))))

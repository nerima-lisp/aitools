(in-package #:aitools.edit.application)

(defun %dry-run-in-tx/k (store tx-id validate &key on-planned on-rejected on-not-found)
  (aitools.store.application:call-with-tx-view/k
   store tx-id
   :on-view (lambda (view)
              (funcall validate view
                       (lambda (requests)
                         (aitools.store.domain:plan-changes/k
                          requests
                          :lookup-state (lambda (path) (aitools.store.application:view-path-state view path))
                          :lookup-content (lambda (path) (aitools.store.application:view-read-file view path))
                          :list-children (lambda (path)
                                           (mapcar (lambda (entry) (%child path (car entry)))
                                                   (aitools.store.application:view-directory-entries view path)))
                          :on-planned on-planned
                          :on-rejected on-rejected))
                       on-rejected))
   :on-not-found on-not-found))

(defun %inspect-write-plan/k (write-plan lock-timeout dry-run argv display-path on-ok on-error)
  "Inspect WRITE-PLAN without reading workspace state and call one continuation."
  (multiple-value-bind (lock-timeout-ms lock-timeout-valid) (%lock-timeout-ms lock-timeout)
    (multiple-value-bind (hash-entries hash-error) (%parse-expect-hashes (write-plan-expect-hashes write-plan))
      (let* ((expect-count-text (write-plan-expect-count write-plan))
             (expect-count (and expect-count-text (parse-count expect-count-text))))
        (cond
         ((not lock-timeout-valid)
          (funcall on-error
                   "argument.invalid"
                   (format nil "--lock-timeout ~S is not a duration (<n>ms|s|m|h|d)" lock-timeout)
                   nil))
         (hash-error (funcall on-error "argument.invalid" hash-error nil))
         ((and expect-count-text (null expect-count))
          (funcall on-error
                   "argument.invalid"
                   (format nil "--expect-count ~S is not a non-negative integer" expect-count-text)
                   nil))
         (t
          (dolist (requirement (write-plan-guard-requirements write-plan))
            (ecase (first requirement)
              (:expect-hash
               (let ((path (second requirement)))
                 ;; A bare --expect-hash is the first target's (VALIDATE hashes
                 ;; it against that one); any other file of a multi-file write
                 ;; needs its own PATH=HASH.
                 (unless (find-if (lambda (entry)
                                    (let ((entry-path (aitools.kernel.domain:expect-hash-entry-path entry)))
                                      (or (null path) (equal path (or entry-path display-path
                                                                      )))))
                                  hash-entries)
                   (return-from %inspect-write-plan/k
                                (funcall on-error
                                         "argument.invalid"
                                         (format nil "this write needs --expect-hash ~:[<hash>~;~:*~A=<hash>~]"
                                                 (and (rest (write-plan-targets write-plan)) path))
                                         (list (repair "get-hash"
                                                       "Read the current hash, then pass that as --expect-hash."
                                                       (format nil "aitools info ~A"
                                                               (aitools.protocol.domain:shell-quote (or path
                                                                                                        display-path
                                                                                                        ))))))))))
              (:expect-count
               (unless (or expect-count dry-run)
                 (return-from %inspect-write-plan/k
                              (funcall on-error
                                       "argument.invalid" "this write needs --expect-count N"
                                       (list (repair "count"
                                                     (concatenate 'string
                                                                  "Count the selection without "
                                                                  "writing, then pass that "
                                                                  "--expect-count."
                                                                  )
                                                     (dry-run-command-line argv)))))))))
          (let ((redacted (find-if #'%contains-placeholder-p (write-plan-inputs write-plan))))
            (when redacted
              (return-from %inspect-write-plan/k
                           (funcall on-error
                                    "refusal.redacted-input"
                                    (format nil "the input contains ~A, an output mask rather than real content"
                                            +redaction-placeholder+)
                                    nil))))
          (funcall on-ok lock-timeout-ms hash-entries expect-count)))))))

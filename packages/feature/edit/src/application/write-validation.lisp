(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
  (%validate-write/k view
                     ports
                     write-plan
                     workspace-root
                     workspace-store
                     hash-entries
                     paths
                     area
                     temporary-root
                     command-line
                     expect-count
                     dry-run
                     tx
                     &key
                     on-committed
                     on-rejected)
  (let* ((host (edit-ports-workspace-host ports))
         (targets (write-plan-targets write-plan))
         (command (write-plan-command write-plan))
         (failure-code nil)
         (failure-message nil)
         (failure-conflicts nil))
    (labels ((hash-path (entry)
               (%resolve-hash-path entry targets paths area host workspace-root temporary-root))
             (hash-view (entry-area)
               (if (or (null entry-area) (eq entry-area area))
                   view
                   (aitools.store.application:disk-view
                    (if (eq entry-area :temporary)
                        (funcall (edit-ports-open-store ports) temporary-root)
                        workspace-store)))))
      (aitools.kernel.domain:flow-call
        (%resolve-targets host workspace-root targets temporary-root)
        (:on-resolved (recheck recheck-area)
                      (declare (ignore recheck-area))
                      (when (and (eq area :inside) (not (equal recheck paths)))
                        (setf failure-code "refusal.target-changed"
                              failure-message "a target path now resolves elsewhere")))
        (:on-error (code message &key path)
                   (declare (ignore path))
                   (setf failure-code code
                         failure-message message)))
      (unless failure-code
        (dolist (entry hash-entries)
          (unless failure-code
            (multiple-value-bind (path entry-area) (hash-path entry)
              (let* ((expected (aitools.kernel.domain:expect-hash-entry-hash entry))
                     (actual (and path (view-hash (hash-view entry-area) path host))))
                (unless (equal expected actual)
                  (setf failure-code "refusal.target-changed"
                        failure-message (format nil
                                                "~A changed: expected hash ~A, found ~A"
                                                path
                                                expected
                                                (or actual "none"))
                        failure-conflicts (list (%hash-conflict path expected actual)))))))))
      (if failure-code
          (if failure-conflicts
              (aitools.kernel.domain:flow-continue
                :on-rejected
                failure-code
                failure-message
                :conflicts
                failure-conflicts)
              (aitools.kernel.domain:flow-continue
                :on-rejected
                failure-code
                failure-message))
          (let ((context
                 (make-write-context :view
                                     view
                                     :root
                                     workspace-root
                                     :host
                                     host
                                     :ports
                                     ports
                                     :paths
                                     paths
                                     :command
                                     command
                                     :command-line
                                     command-line
                                     :expect-count
                                     expect-count
                                     :temporary-root
                                     temporary-root
                                     :hash-paths
                                     (mapcar #'hash-path hash-entries)
                                     :dry-run
                                     dry-run
                                     :tx
                                     tx)))
            (aitools.kernel.domain:flow-call
              (run-plan context (write-plan-plan write-plan))
              (:on-committed (requests more)
                             (let ((redacted (%redacted-output-path view requests)))
                               (if redacted
                                   (aitools.kernel.domain:flow-continue
                                     :on-rejected
                                     "refusal.redacted-input"
                                     (format nil
                                             (concatenate (quote string)
                                                          "the write would put "
                                                          "~A into ~A, which "
                                                          "currently has none; "
                                                          "~A is an output mask, "
                                                          "not real content")
                                             +redaction-placeholder+
                                             redacted
                                             +redaction-placeholder+))
                                   (aitools.kernel.domain:flow-continue
                                     :on-committed
                                     requests
                                     (append more (%selected-count-field write-plan context))))))
              (:on-rejected (code message &key candidates diagnostics conflicts repairs path)
                            (aitools.kernel.domain:flow-continue
                              :on-rejected
                              code
                              message
                              :candidates candidates
                              :diagnostics diagnostics
                              :conflicts conflicts
                              :repairs repairs
                              :path path))))))))
(aitools.kernel.domain:define-flow
  (%plan-write-changes/k view requests extra &key on-planned on-rejected)
  (aitools.store.domain:plan-changes/k
    requests
    :lookup-state
    (lambda (path)
      (aitools.store.application:view-path-state view path))
    :lookup-content
    (lambda (path)
      (aitools.store.application:view-read-file view path))
    :list-children
    (lambda (path)
      (mapcar
        (lambda (entry)
          (%child path (car entry)))
        (aitools.store.application:view-directory-entries view path)))
    :on-planned
    (lambda (results)
      (aitools.kernel.domain:flow-continue :on-planned results extra))
    :on-rejected
    (lambda (code message &key candidates diagnostics conflicts repairs)
      (aitools.kernel.domain:flow-continue
        :on-rejected
        code
        message
        :candidates candidates
        :diagnostics diagnostics
        :conflicts conflicts
        :repairs repairs))))

(aitools.kernel.domain:define-flow
  (%dry-run-in-tx/k store
                    tx-id
                    ports
                    write-plan
                    workspace-root
                    workspace-store
                    hash-entries
                    paths
                    area
                    temporary-root
                    command-line
                    expect-count
                    dry-run
                    tx
                    &key
                    on-planned
                    on-rejected
                    on-not-found)
  (aitools.store.application:call-with-tx-view/k store
                                                 tx-id
                                                 :on-view
                                                 (lambda (view)
                                                   (aitools.kernel.domain:flow-call
                                                     (%validate-write/k view
                                                                        ports
                                                                        write-plan
                                                                        workspace-root
                                                                        workspace-store
                                                                        hash-entries
                                                                        paths
                                                                        area
                                                                        temporary-root
                                                                        command-line
                                                                        expect-count
                                                                        dry-run
                                                                        tx)
                                                     (:on-committed (requests extra)
                                                                    (aitools.kernel.domain:flow-call
                                                                     (%plan-write-changes/k view requests extra)
                                                                     (:on-planned (planned-requests planned-extra)
                                                                                  (aitools.kernel.domain:flow-continue
                                                                                   :on-planned
                                                                                   planned-requests
                                                                                   planned-extra))
                                                                     (:on-rejected
                                                                      (code message
                                                                            &key
                                                                            candidates
                                                                            diagnostics
                                                                            conflicts
                                                                            repairs)
                                                                      (aitools.kernel.domain:flow-continue
                                                                       :on-rejected
                                                                       code
                                                                       message
                                                                       :candidates candidates
                                                                       :diagnostics diagnostics
                                                                       :conflicts conflicts
                                                                       :repairs repairs))))
                                                     (:on-rejected (code message &key candidates diagnostics conflicts repairs path)
                                                      (aitools.kernel.domain:flow-continue
                                                        :on-rejected code message
                                                        :candidates candidates
                                                        :diagnostics diagnostics
                                                        :conflicts conflicts
                                                        :repairs repairs
                                                        :path path))))
                                                 :on-not-found
                                                 (lambda ()
                                                   (aitools.kernel.domain:flow-continue
                                                     :on-not-found))))

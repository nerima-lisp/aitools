(in-package #:aitools.edit.application)

(define-write-command "copy" (ports env positionals options &key on-plan on-error)
  (let ((max-bytes
         (handler-case (aitools.kernel.domain:size-bytes
                        (aitools.kernel.domain:parse-size (or (getf options :max-bytes) "1GiB")))
           (error ()
             nil))))
    (cond
      ((/= (length positionals) 2)
       (flow-continue
         :on-error
         "argument.invalid"
         "copy takes SRC and DST"))
      ((null max-bytes)
       (flow-continue
         :on-error
         "argument.invalid"
         (format
           nil
           "--max-bytes ~S is not a size"
           (getf options :max-bytes))))
      (t
       (flow-continue
         :on-plan
         (make-write-plan
           :command
           "copy"
           :targets
           (list (make-write-target (first positionals)) (make-write-target (second positionals) :follow nil))
           :expect-hashes
           (getf options :expect-hash)
           :plan
           (lambda (context &key on-committed on-rejected)
             (let* ((source (context-path context 0))
                    (destination (context-path context 1))
                    (state (%state context source))
                    (kind (aitools.store.domain:entry-state-kind state)))
               (case kind
                 (:absent
                  (aitools.kernel.domain:flow-call
                    (%not-found context source)
                    (:on-error (code message &key candidates diagnostics conflicts repairs path)
                               (declare (ignore path))
                               (funcall on-rejected
                                        code
                                        message
                                        :candidates
                                        candidates
                                        :diagnostics
                                        diagnostics
                                        :conflicts
                                        conflicts
                                        :repairs
                                        repairs))))
                 (:directory
                  (if (not (getf options :recursive))
                      (funcall on-rejected
                               "argument.invalid"
                               (format
                                 nil
                                 (concatenate 'string "~A is a directory; copying " "it needs --recursive")
                                 source)
                               :repairs
                               (list
                                (repair "recursive"
                                        "Copy the directory tree."
                                        (concatenate 'string (write-context-command-line context) " --recursive"))))
                      (aitools.kernel.domain:flow-call
                        (%destination-check/k context destination kind nil)
                        (:on-rejected (code message &key candidates diagnostics conflicts repairs path)
                                      (declare (ignore path))
                                      (funcall on-rejected
                                               code
                                               message
                                               :candidates
                                               candidates
                                               :diagnostics
                                               diagnostics
                                               :conflicts
                                               conflicts
                                               :repairs
                                               repairs))
                        (:on-ok ()
                                (aitools.kernel.domain:flow-call
                                  (%copy-tree-requests/k context source destination max-bytes)
                                  (:on-error (code message &key candidates diagnostics conflicts repairs path)
                                             (declare (ignore path))
                                             (funcall on-rejected
                                                      code
                                                      message
                                                      :candidates
                                                      candidates
                                                      :diagnostics
                                                      diagnostics
                                                      :conflicts
                                                      conflicts
                                                      :repairs
                                                      repairs))
                                  (:on-requests (requests files skipped)
                                                (funcall on-committed
                                                         requests
                                                         (list (cons "files" files) (cons "skipped" skipped)))))))))
                 (:file
                  (aitools.kernel.domain:flow-call
                    (%destination-check/k context destination kind (getf options :overwrite))
                    (:on-rejected (code message &key candidates diagnostics conflicts repairs path)
                                  (declare (ignore path))
                                  (funcall on-rejected
                                           code
                                           message
                                           :candidates
                                           candidates
                                           :diagnostics
                                           diagnostics
                                           :conflicts
                                           conflicts
                                           :repairs
                                           repairs))
                    (:on-ok ()
                            (let ((content
                                   (aitools.store.application:view-read-file (write-context-view context) source)))
                              (if (> (length content) max-bytes)
                                  (funcall on-rejected
                                           "refusal.too-large"
                                           (format
                                             nil
                                             "~A exceeds --max-bytes ~D"
                                             source
                                             max-bytes))
                                  (funcall on-committed
                                           (list
                                            (aitools.store.domain:write-file-request
                                              destination
                                              content
                                              :mode
                                              (aitools.store.domain:entry-state-mode state)))))))))
                 (t
                  (funcall on-rejected
                           "refusal.not-a-file"
                           (format
                             nil
                             "~A is a ~(~A~)"
                             source
                             kind))))))
           :record-options
           options
           :record-positionals
           (lambda (paths)
             paths)))))))


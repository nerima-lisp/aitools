(in-package #:aitools.edit.application)

(define-write-command "copy"
                      (ports env positionals options on-plan fail)
                      (let ((max-bytes
                             (handler-case (aitools.kernel.domain:size-bytes
                                            (aitools.kernel.domain:parse-size
                                             (or (getf options :max-bytes)
                                                 "1GiB")))
                             (error ()
                             nil))))
                      (cond
                      ((/= (length positionals) 2)
                       (funcall fail
                                "argument.invalid"
                                "copy takes SRC and DST"))
                      ((null max-bytes)
                       (funcall fail
                                "argument.invalid"
                                (format nil
                                        "--max-bytes ~S is not a size"
                                        (getf options :max-bytes))))
                      (t
                       (funcall on-plan
                                (make-write-plan :command
                                                 "copy"
                                                 :targets
                                                 (list
                                                  (make-write-target
                                                   (first positionals))
                                                  (make-write-target
                                                   (second positionals)
                                                   :follow
                                                   nil))
                                                 :expect-hashes
                                                 (getf options :expect-hash)
                                                 :plan
                                                 (lambda (context commit reject)
                                                 (let* ((source
                                                         (context-path context
                                                                       0))
                                                        (destination
                                                         (context-path context
                                                                       1))
                                                        (state
                                                         (%state context source))
                                                        (kind
                                                         (aitools.store.domain:entry-state-kind
                                                          state)))
                                                 (case kind
                                                 (:absent
                                                  (%not-found context
                                                              source
                                                              reject))
                                                 (:directory
                                                  (if (not
                                                       (getf options :recursive))
                                                  (funcall reject
                                                           "argument.invalid"
                                                           (format nil
                                                                   "~A is a directory; copying it needs --recursive"
                                                                   source)
                                                           :repairs
                                                           (list
                                                            (repair "recursive"
                                                                    "Copy the directory tree."
                                                                    (concatenate
                                                                     'string
                                                                     (write-context-command-line
                                                                      context)
                                                                     " --recursive"))))
                                                  (%destination-check/k context
                                                                        destination
                                                                        kind
                                                                        nil
                                                                        reject
                                                                        (lambda
                                                                        ()
                                                                        (%copy-tree-requests/k
                                                                         context
                                                                         source
                                                                         destination
                                                                         max-bytes
                                                                         reject
                                                                         (lambda
                                                                         (requests
                                                                          files
                                                                          skipped)
                                                                         (funcall
                                                                          commit
                                                                          requests
                                                                          (list
                                                                           (cons
                                                                            "files"
                                                                            files)
                                                                           (cons
                                                                            "skipped"
                                                                            skipped)))))))))
                                                 (:file
                                                  (%destination-check/k context
                                                                        destination
                                                                        kind
                                                                        (getf
                                                                         options
                                                                         :overwrite)
                                                                        reject
                                                                        (lambda
                                                                        ()
                                                                        (let ((content
                                                                               (aitools.store.application:view-read-file
                                                                                (write-context-view
                                                                                 context)
                                                                                source)))
                                                                        (if (>
                                                                             (length
                                                                              content)
                                                                             max-bytes)
                                                                        (funcall
                                                                         reject
                                                                         "refusal.too-large"
                                                                         (format
                                                                          nil
                                                                          "~A exceeds --max-bytes ~D"
                                                                          source
                                                                          max-bytes))
                                                                        (funcall
                                                                         commit
                                                                         (list
                                                                          (aitools.store.domain:write-file-request
                                                                           destination
                                                                           content
                                                                           :mode
                                                                           (aitools.store.domain:entry-state-mode
                                                                            state)))))))))
                                                 (t
                                                  (funcall reject
                                                           "refusal.not-a-file"
                                                           (format nil
                                                                   "~A is a ~(~A~)"
                                                                   source
                                                                   kind))))))
                                                 :record-options
                                                 options
                                                 :record-positionals
                                                 (lambda (paths)
                                                 paths)))))))


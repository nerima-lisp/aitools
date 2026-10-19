(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (%read-target-error/k context target problem &key on-error)
 (declare (type function on-error))
 (if (eq problem :unreadable)
     (aitools.kernel.domain:flow-continue :on-error
                                          "environment.io"
                                          (format nil
                                                  "file ~A cannot be read: permission denied"
                                                  (file-target-argument target))
                                          :repairs
                                          (list
                                           (repair "describe"
                                                   "Show the file's mode and owner to fix its permissions."
                                                   (command-line context
                                                                 (list "info"
                                                                       (file-target-argument
                                                                        target))
                                                                 :tx
                                                                 :none))))
     (aitools.kernel.domain:flow-continue :on-error
                                          "input.not-found"
                                          (format nil
                                                  "file ~A does not exist"
                                                  (file-target-argument target))
                                          :candidates
                                          (missing-path-candidates context
                                                                   (file-target-absolute
                                                                    target))
                                          :repairs
                                          (list
                                           (repair "find"
                                                   "List nearby files."
                                                   (command-line context
                                                                 (list "find"
                                                                       (path-basename
                                                                        (file-target-absolute
                                                                         target)))
                                                                 :tx
                                                                 :none))))))

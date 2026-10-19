(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (%archive-read-item context
                     path
                     entry
                     archive
                     item
                     selector
                     mode
                     max-lines
                     &key
                     on-ok
                     on-partial
                     on-error)
 (declare (type function on-ok on-partial on-error))
 (aitools.kernel.domain:flow-call
  (archive-item-content/k archive item)
  (:on-content (content)
               (if (eq mode :hex)
                   (aitools.kernel.domain:flow-call
                    (%archive-hex context
                                  path
                                  entry
                                  content
                                  max-lines)
                    (:on-ok (fields)
                            (aitools.kernel.domain:flow-continue
                             :on-ok
                             fields))
                    (:on-partial (fields)
                                 (aitools.kernel.domain:flow-continue
                                  :on-partial
                                  fields)))
                 (aitools.kernel.domain:flow-call
                  (%archive-text context
                                 path
                                 entry
                                 content
                                 selector
                                 max-lines)
                  (:on-ok (fields)
                          (aitools.kernel.domain:flow-continue
                           :on-ok
                           fields))
                  (:on-partial (fields)
                               (aitools.kernel.domain:flow-continue
                                :on-partial
                                fields))
                  (:on-error (code message &key candidates diagnostics conflicts repairs)
                             (aitools.kernel.domain:flow-continue
                              :on-error code message
                              :candidates candidates
                              :diagnostics diagnostics
                              :conflicts conflicts
                              :repairs repairs)))))
  (:on-too-large (limit)
                 (aitools.kernel.domain:flow-continue
                  :on-error
                  "refusal.too-large"
                  (format nil
                          "entry ~A of ~A is larger than ~D bytes"
                          (archive-item-name
                           item)
                          path
                          limit)
                  :repairs
                  (list
                   (repair "list"
                           "List the archive's entries."
                           (%archive-list-command
                            context
                            path)))))
  (:on-malformed (reason)
                 (aitools.kernel.domain:flow-continue
                  :on-error
                  "input.unsupported-format"
                  (format nil
                          "entry ~A of ~A cannot be decoded: ~A"
                          (archive-item-name
                           item)
                          path
                          reason)
                  :repairs
                  (list
                   (repair "list"
                           "List the archive's entries."
                           (%archive-list-command
                            context
                            path)))))))

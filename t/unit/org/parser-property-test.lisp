(in-package #:aitools.org.test)

(describe "aitools.org infrastructure parser and source codec"
          (it-property "preserves generated UTF-8 paragraph bytes"
                       ((text
                         (gen-string :min-length
                                     0
                                     :max-length
                                     80
                                     :alphabet
                                     "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,.-_あ")))
                       (let ((octets (aitools.test.support:string-bytes text)))
                         (multiple-value-bind (document failure)
                             (parse-org-octets octets
                                               :source-name
                                               "generated.org")
                           (expect failure :to-be nil)
                           (expect (aitools.org.domain::org-document-p document)
                                   :to-be
                                   t)
                           (expect
                            (aitools.org.domain::org-document-source-name
                             document)
                            :to-equal
                            "generated.org")
                           (expect
                            (aitools.org.domain::org-document-octets document)
                            :to-equalp
                            octets)))))

(cl-weave:describe "aitools.org infrastructure query diagnostics"
                   (cl-weave:it
                    "maps query offset boundaries to clamped one-based positions"
                    (let ((match (format nil "one~%bad")))
                      (dolist
                          (case (quote ((0 1 1) (4 2 1) (7 2 4) (10 2 4))))
                        (destructuring-bind (offset line column) case
                          (let ((failure
                                 (aitools.org.infrastructure::%query-syntax-failure
                                  match
                                  (make-condition 'org-kit:org-query-error
                                                  :position
                                                  offset))))
                            (expect
                             (aitools.org.domain:org-failure-line failure)
                             :to-be
                             line)
                            (expect
                             (aitools.org.domain:org-failure-column failure)
                             :to-be
                             column)
                            (expect
                             (aitools.org.domain:org-failure-offset failure)
                             :to-be
                             offset)))))))


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
                           (expect (aitools.org.domain:org-document-p document)
                                   :to-be
                                   t)
                           (expect
                            (aitools.org.domain:org-document-source-name
                             document)
                            :to-equal
                            "generated.org")
                           (expect
                            (aitools.org.domain:org-document-octets document)
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

(cl-weave:describe "aitools.org infrastructure parser refusals"
                   (cl-weave:it "rejects UTF-16 and UTF-32 BOMs and NUL bytes"
                                (dolist
                                    (case (list
                                           (list #(255 254 65 0)
                                                 "UTF-16 and UTF-32 Org input is unsupported")
                                           (list #(254 255 0 65)
                                                 "UTF-16 and UTF-32 Org input is unsupported")
                                           (list #(255 254 0 0)
                                                 "UTF-16 and UTF-32 Org input is unsupported")
                                           (list #(0 0 254 255)
                                                 "UTF-16 and UTF-32 Org input is unsupported")
                                           (list #(65 0)
                                                 "Org input contains a NUL byte")))
                                  (multiple-value-bind (document failure)
                                      (aitools.org.infrastructure:parse-org-octets
                                       (first case))
                                    (expect document :to-be nil)
                                    (expect
                                     (aitools.org.domain:org-failure-kind
                                      failure)
                                     :to-be
                                     :unsupported)
                                    (expect
                                     (aitools.org.domain:org-failure-message
                                      failure)
                                     :to-equal
                                     (second case))))))

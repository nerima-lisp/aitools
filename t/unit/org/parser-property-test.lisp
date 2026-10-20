(in-package #:aitools.org.test)

(describe "aitools.org infrastructure parser and source codec"
  (it-property "preserves generated UTF-8 paragraph bytes"
      ((text (gen-string :min-length 0 :max-length 80
                         :alphabet "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,.-_あ")))
    (let ((octets (aitools.test.support:string-bytes text)))
      (multiple-value-bind (document failure)
          (parse-org-octets octets)
        (expect failure :to-be nil)
        (expect (aitools.org.domain:org-document-octets document) :to-equalp octets)))))
(cl-weave:describe "aitools.org infrastructure query diagnostics"
  (cl-weave:it "maps query offsets after a newline to one-based positions"
    (let* ((match (format nil "one~%bad"))
           (condition (make-condition 'org-kit:org-query-error :position 5))
           (failure
             (aitools.org.infrastructure::%query-syntax-failure
              match condition)))
      (expect (aitools.org.domain:org-failure-line failure) :to-be 2)
      (expect (aitools.org.domain:org-failure-column failure) :to-be 2))))

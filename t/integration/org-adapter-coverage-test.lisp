(in-package #:aitools.edit.test)

(defmacro %with-org-kit-condition (function condition &body body)
  `(cl-weave:with-mocked-functions
       (((symbol-function ',function)
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (error ,condition))))
     ,@body))

(describe "Org infrastructure adapter boundary outcomes"
  (it "maps generic parser errors to internal failures"
    (let ((octets (sb-ext:string-to-octets "* Entry" :external-format :utf-8)))
      (%with-org-kit-condition
          org-kit:parse-org
          (make-condition 'org-kit:org-kit-error :message "parser failed")
        (multiple-value-bind (document failure)
            (aitools.org.infrastructure:parse-org-octets
             octets :source-name "parser.org")
          (expect document :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :internal)
          (expect (aitools.org.domain:org-failure-message failure)
                  :to-equal "parser failed")))))
  (it "maps outline resource and kit failures"
    (let ((document (%org-coverage-document "* Entry")))
      (%with-org-kit-condition
          org-kit:outline
          (make-condition 'org-kit:org-resource-limit-error
                          :message "outline limit"
                          :limit-name :max-output-length :limit 1 :actual 2)
        (multiple-value-bind (outline failure)
            (aitools.org.infrastructure::outline-org-document document 1)
          (expect outline :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :too-large)
          (expect (aitools.org.domain:org-failure-message failure)
                  :to-equal "outline limit")))
      (%with-org-kit-condition
          org-kit:outline
          (make-condition 'org-kit:org-kit-error :message "outline failed")
        (multiple-value-bind (outline failure)
            (aitools.org.infrastructure::outline-org-document document 1)
          (expect outline :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :internal)
          (expect (aitools.org.domain:org-failure-message failure)
                  :to-equal "outline failed")))))
  (it "normalizes link types supplied as strings and symbols"
    (expect (aitools.org.infrastructure::%link-type-name "HTTPS")
            :to-equal "https")
    (expect (aitools.org.infrastructure::%link-type-name 'mailto)
            :to-equal "mailto")
    (expect (aitools.org.infrastructure::%link-type-name '(unknown))
            :to-be nil))
  (it "maps export resource failures and unsupported syntax locations"
    (let ((document (%org-coverage-document "* Entry")))
      (%with-org-kit-condition
          org-kit:export-org
          (make-condition 'org-kit:org-resource-limit-error
                          :message "export limit"
                          :limit-name :max-output-length :limit 1 :actual 2)
        (multiple-value-bind (content failure)
            (aitools.org.infrastructure::export-org-document document :gfm)
          (expect content :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :too-large)
          (expect (aitools.org.domain:org-failure-message failure)
                  :to-equal "export limit")))
      (%with-org-kit-condition
          org-kit:export-org
          (make-condition 'org-kit:org-export-unsupported-syntax
                          :message "syntax unsupported" :position 2)
        (multiple-value-bind (content failure)
            (aitools.org.infrastructure::export-org-document document :gfm)
          (expect content :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :unsupported)
          (expect (aitools.org.domain:org-failure-line failure) :to-be 1)
          (expect (aitools.org.domain:org-failure-column failure) :to-be 3)
          (expect (aitools.org.domain:org-failure-offset failure) :to-be 2)))))
  (it "maps unsupported export syntax without a position and backend failures"
    (let ((document (%org-coverage-document "* Entry")))
      (%with-org-kit-condition
          org-kit:export-org
          (make-condition 'org-kit:org-export-unsupported-syntax
                          :message "syntax unsupported"
                          :position nil)
        (multiple-value-bind (content failure)
            (aitools.org.infrastructure::export-org-document document :gfm)
          (expect content :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :unsupported)
          (expect (aitools.org.domain:org-failure-message failure)
                  :to-equal "syntax unsupported")
          (expect (aitools.org.domain:org-failure-line failure) :to-be nil)
          (expect (aitools.org.domain:org-failure-offset failure) :to-be nil)))
      (%with-org-kit-condition
          org-kit:export-org
          (make-condition 'org-kit:org-export-error :message "backend failed")
        (multiple-value-bind (content failure)
            (aitools.org.infrastructure::export-org-document document :gfm)
          (expect content :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :unsupported)
          (expect (aitools.org.domain:org-failure-message failure)
                  :to-equal "backend failed")))))
  (it "maps other export kit failures to internal failures"
    (let ((document (%org-coverage-document "* Entry")))
      (%with-org-kit-condition
          org-kit:export-org
          (make-condition 'org-kit:org-kit-error :message "export failed")
        (multiple-value-bind (content failure)
            (aitools.org.infrastructure::export-org-document document :gfm)
          (expect content :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :internal)
          (expect (aitools.org.domain:org-failure-message failure)
                  :to-equal "export failed")))))
)

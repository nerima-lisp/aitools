(in-package #:aitools.integration.org-dispatch-test)

(defun %write-sparse-oversized-org-file (path limit)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type (quote (unsigned-byte 8)))
    (write-sequence
     (make-array (min 8192 limit)
                 :element-type (quote (unsigned-byte 8))
                 :initial-element 65)
     stream)
    (unless (file-position stream limit)
      (error "Could not seek to the production Org input limit"))
    (write-byte 65 stream)
    (finish-output stream)
    (expect (file-length stream) :to-be (1+ limit))))

(describe "Org read error envelopes through the loaded dispatcher"
  (it "rejects an unresolved root without outline result fields"
    (aitools.test.support:with-workspace (root state)
      (declare (ignore state))
      (let ((missing-root (concatenate (quote string) root "/missing-root")))
        (multiple-value-bind (code envelope stream)
            (aitools.test.support:run-aitools
             missing-root (list "org" "outline" "unused.org"))
          (%expect-error-envelope
           code envelope stream "org outline" "input.not-found"
           (list "entries" "total" "truncated"))
          (expect
           (aitools.test.support:envelope-value envelope "error" "message")
           :to-equal
           (format nil "workspace root ~A does not exist" missing-root))))))
  (it "refuses UTF-8 input above the production limit after an ASCII sniff"
    (aitools.test.support:with-workspace (root state)
      (declare (ignore state))
      (let* ((path (concatenate (quote string) root "/large.org"))
             (limit (org-kit:limit-value (org-kit:make-limits) :max-input-length)))
        (%write-sparse-oversized-org-file path limit)
        (multiple-value-bind (code envelope stream)
            (aitools.test.support:run-aitools
             root (list "org" "outline" "large.org"))
          (%expect-error-envelope
           code envelope stream "org outline" "refusal.too-large"
           (list "entries" "total" "truncated"))
          (expect (search "exceeds the Org input limit"
                          (aitools.test.support:envelope-value
                           envelope "error" "message"))
                  :to-be-truthy))
        (with-open-file (stream path :direction :input
                                     :element-type (quote (unsigned-byte 8)))
          (let ((prefix (make-array 8192
                                    :element-type (quote (unsigned-byte 8)))))
            (expect (read-sequence prefix stream) :to-be (length prefix))
            (expect (every (lambda (byte) (= byte 65)) prefix) :to-be t)
            (expect (file-position stream limit) :to-be t)
            (expect (read-byte stream nil nil) :to-be 65)
            (expect (file-length stream) :to-be (1+ limit))))))))

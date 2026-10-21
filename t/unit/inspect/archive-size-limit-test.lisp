(in-package #:aitools.inspect.test)

(describe "archive gzip decompression limits"
  (it "rejects oversized plain gzip member content after opening"
    (let* ((gzip (cdr (assoc "notes.txt.gz" (sample-archives) :test (function string=))))
           (decoded (make-array (1+ +archive-max-entry+)
                                :element-type (quote (unsigned-byte 8))
                                :initial-element 0))
           (archive nil)
           (item nil))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.text.domain:gzip-decompress))
            (lambda (octets &key max-output)
              (declare (ignore octets))
              (expect max-output :to-be +archive-max-decompressed+)
              decoded)))
        (aitools.inspect.domain:open-archive/k
         gzip "notes.txt.gz"
         :on-archive (lambda (opened) (setf archive opened))
         :on-unsupported (lambda () (fail "valid gzip was not recognized"))
         :on-malformed (lambda (reason)
                         (fail (format nil "valid gzip was malformed: ~A" reason)))
         :on-too-large (lambda (limit)
                         (fail (format nil "gzip was limited at ~D bytes" limit))))
      (setf item (first (aitools.inspect.domain:archive-items archive)))
      (expect (aitools.inspect.domain:archive-format archive) :to-be :gz)
      (expect (aitools.inspect.domain:archive-item-name item) :to-equal "notes.txt")
      (expect (aitools.inspect.domain:archive-item-size item) :to-be (length decoded))
      (aitools.inspect.domain:archive-item-content/k
       archive item
       :on-content (lambda (content)
                     (fail (format nil "oversized gzip member returned ~D bytes"
                                   (length content))))
       :on-too-large (lambda (limit) (expect limit :to-be +archive-max-entry+))
       :on-malformed (lambda (reason)
                       (fail (format nil "oversized content was malformed: ~A" reason)))))))
  (it "returns plain gzip member content at the entry limit"
    (let* ((gzip (cdr (assoc "notes.txt.gz" (sample-archives) :test (function string=))))
           (decoded (make-array +archive-max-entry+
                                :element-type (quote (unsigned-byte 8))
                                :initial-element #x5A))
           (archive nil)
           (item nil))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.text.domain:gzip-decompress))
            (lambda (octets &key max-output)
              (declare (ignore octets))
              (expect max-output :to-be +archive-max-decompressed+)
              decoded)))
        (aitools.inspect.domain:open-archive/k
         gzip "notes.txt.gz"
         :on-archive (lambda (opened) (setf archive opened))
         :on-unsupported (lambda () (fail "valid gzip was not recognized"))
         :on-malformed (lambda (reason)
                         (fail (format nil "valid gzip was malformed: ~A" reason)))
         :on-too-large (lambda (limit)
                         (fail (format nil "gzip was limited at ~D bytes" limit))))
      (setf item (first (aitools.inspect.domain:archive-items archive)))
      (expect (aitools.inspect.domain:archive-format archive) :to-be :gz)
      (expect (aitools.inspect.domain:archive-item-name item) :to-equal "notes.txt")
      (expect (aitools.inspect.domain:archive-item-size item) :to-be +archive-max-entry+)
      (aitools.inspect.domain:archive-item-content/k
       archive item
       :on-content (lambda (content) (expect content :to-equalp decoded))
       :on-too-large (lambda (limit)
                       (fail (format nil "content at the limit was rejected at ~D bytes" limit)))
       :on-malformed (lambda (reason)
                       (fail (format nil "content at the limit was malformed: ~A" reason)))))))
  (it "uses the configured decompression limit for tar.gz archives"
    (let ((tar-gz
            (cdr (assoc "a.tar.gz"
                        (sample-archives)
                        :test (function string=)))))
      (aitools.inspect.domain:open-archive/k
       tar-gz
       "a.tar.gz"
       :max-gzip-output 1
       :on-archive (lambda (archive)
                     (declare (ignore archive))
                     (fail "tar.gz exceeded the configured decompression limit"))
       :on-unsupported
       (lambda () (fail "valid tar.gz was not recognized"))
       :on-malformed
       (lambda (reason)
         (fail (format nil "valid tar.gz was malformed: ~A" reason)))
       :on-too-large
       (lambda (limit) (expect limit :to-be 1))))))

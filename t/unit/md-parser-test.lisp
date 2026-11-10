(in-package #:aitools.md.test)

(describe "Markdown parser source spans"
          (it "selects ATX and setext headings while ignoring front matter and fenced code"
              (let ((source
                     (format nil
                             "---~%title: demo~%---~%# Same~%body~%~~~~~~lisp~%# hidden~%~~~~~~~~~%## Child~%nested~%~%Same~%===~%tail~%")))
                (multiple-value-bind (document code message offset)
                    (aitools.md.infrastructure:parse-md-octets (sb-ext:string-to-octets source :external-format :utf-8))
                  (declare (ignore message offset))
                  (expect code :to-be nil)
                  (let ((headings (aitools.md.domain:md-document-headings document)))
                    (expect (mapcar #'aitools.md.domain:md-heading-heading headings) :to-equal '("Same" "Child" "Same"))
                    (expect (mapcar #'aitools.md.domain:md-heading-level headings) :to-equal '(1 2 1))
                    (expect (mapcar #'aitools.md.domain:md-heading-line headings) :to-equal '(4 9 12))
                    (expect (mapcar #'aitools.md.domain:md-heading-section headings) :to-equal '(1 2 3))))))
          (it "maps character spans to UTF-8 byte boundaries and splices only the selected bytes"
              (let* ((source
                      (format nil
                              "preamble 🚀~C~C# A~C~Cé body 漢🧪~C~C## Child~C~Cinside~C~C# Next 🚀~C~Ctail"
                              #\Return
                              #\Newline
                              #\Return
                              #\Newline
                              #\Return
                              #\Newline
                              #\Return
                              #\Newline
                              #\Return
                              #\Newline
                              #\Return
                              #\Newline))
                     (original (sb-ext:string-to-octets source :external-format :utf-8))
                     (replacement
                      (sb-ext:string-to-octets (format nil "# A revised~%replacement 漢字~%") :external-format :utf-8)))
                (multiple-value-bind (document code message offset) (aitools.md.infrastructure:parse-md-octets original)
                  (declare (ignore message offset))
                  (expect code :to-be nil)
                  (let* ((heading (aitools.md.domain:md-document-heading document 1))
                         (next-heading (aitools.md.domain:md-document-heading document 3))
                         (start (aitools.md.domain:md-heading-start-char heading))
                         (end (aitools.md.domain:md-heading-start-char next-heading))
                         (start-byte (length (sb-ext:string-to-octets (subseq source 0 start) :external-format :utf-8)))
                         (end-byte (length (sb-ext:string-to-octets (subseq source 0 end) :external-format :utf-8)))
                         (expected-section (subseq original start-byte end-byte))
                         (expected
                          (sb-ext:string-to-octets
                           (concatenate 'string
                                        (subseq source 0 start)
                                        (sb-ext:octets-to-string replacement :external-format :utf-8)
                                        (subseq source end))
                           :external-format
                           :utf-8))
                         (section (aitools.md.infrastructure::section-bytes document heading))
                         (updated (aitools.md.infrastructure::splice-section-bytes document heading replacement)))
                    (expect section :to-equalp expected-section)
                    (expect updated :to-equalp expected)))))
          (it "reports invalid UTF-8 with the catalog error code"
              (multiple-value-bind (document code message offset)
                  (aitools.md.infrastructure:parse-md-octets (make-array 4 :element-type (quote (unsigned-byte 8)) :initial-contents (quote (35 32 255 10))))
                (declare (ignore document message))
                (expect code :to-equal "input.not-utf8")
                (expect offset :to-be 2)))
          (it "rejects Markdown nesting beyond the parser limit"
              (let* ((depth (1+ cl-markdown-kit::+default-max-nesting-depth+))
                     (source
                       (with-output-to-string (output)
                         (loop repeat depth
                               do (write-string "> " output))
                         (write-string "nested" output)))
                     (octets
                       (sb-ext:string-to-octets source :external-format :utf-8)))
                (multiple-value-bind (document code message offset)
                    (aitools.md.infrastructure:parse-md-octets octets)
                  (declare (ignore document))
                  (expect code :to-equal "refusal.too-large")
                  (expect message :to-equal "maximum block nesting depth exceeded")
                  (expect offset :to-be nil))))
          (it "rejects valid UTF-16 Markdown before converting source spans"
              (let* ((source (format nil "# UTF-16~%body~%"))
                     (encoded (sb-ext:string-to-octets source :external-format :utf-16le))
                     (element-type (array-element-type encoded))
                     (octets (make-array (+ 2 (length encoded))
                                         :element-type element-type)))
                (setf (aref octets 0) 255
                      (aref octets 1) 254)
                (replace octets encoded :start1 2)
                (multiple-value-bind (document code message offset)
                    (aitools.md.infrastructure:parse-md-octets octets)
                  (declare (ignore document message offset))
                  (expect code :to-equal "input.not-utf8"))))
          (it "rejects octets beyond the application input limit"
              (let ((octets
                      (make-array (1+ aitools.md.application:+md-max-input-bytes+)
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 32)))
                (multiple-value-bind (document code message offset)
                    (aitools.md.infrastructure:parse-md-octets octets)
                  (declare (ignore document))
                  (expect code :to-equal "refusal.too-large")
                  (expect message :to-be nil)
                  (expect offset :to-be nil)))))

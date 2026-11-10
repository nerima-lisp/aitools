(in-package #:aitools.text.test)

(defun %line-test-text (template)
  (substitute #\Return #\| (format nil template)))

(defun %line-test-octets (template bom-p)
  (let ((body (string-bytes (%line-test-text template))))
    (if bom-p
        (join-octets (octets #xEF #xBB #xBF) body)
        body)))

(defun %line-test-lines (templates)
  (coerce (loop for template across templates
                collect (%line-test-text template))
          'simple-vector))

(describe "aitools.text.domain line conversion"
  (it-each (("empty input" "" #() nil)
            ("one unterminated line" "plain" #("plain") nil)
            ("multiple LF-separated lines" "one~%two" #("one" "two") nil)
            ("a final LF does not add an empty line" "one~%" #("one") nil)
            ("an empty line between content lines" "one~%~%three" #("one" "" "three") nil)
            ("a blank line after a final LF" "one~%~%" #("one" "") nil)
            ("CRLF terminators are stripped by default" "one|~%two|~%" #("one" "two") nil)
            ("CRLF terminators can be preserved" "one|~%two|~%" #("one|" "two|") t)
            ("a lone CR is not a line break" "one|two" #("one|two") nil))
    "splits ~A"
    (description input expected keep-cr)
    (declare (ignore description))
    (let ((lines (split-text-lines (%line-test-text input) :keep-cr keep-cr)))
      (expect (typep lines 'simple-vector) :to-be t)
      (expect lines :to-equalp (%line-test-lines expected))))

  (it-each (("empty input" "" nil nil #() :none nil)
            ("one unterminated line" "plain" nil nil #("plain") :none nil)
            ("LF-separated lines with a final terminator" "one~%two~%" nil nil #("one" "two") :lf t)
            ("CRLF-separated lines" "one|~%two|~%" nil nil #("one" "two") :crlf t)
            ("mixed CRLF and LF lines" "one|~%two~%three|~%last" nil nil #("one" "two" "three" "last") :mixed nil)
            ("BOM is excluded from the first line" "one~%two" t nil #("one" "two") :lf nil)
            ("a file containing only a BOM has no lines" "" t nil #() :none nil)
            ("kept CRLF terminators remain in the line strings" "one|~%two|~%" nil t #("one|" "two|") :crlf t))
    "decodes ~A"
    (description input bom-p keep-cr expected ending final-newline)
    (declare (ignore description))
    (multiple-value-bind (lines layout encoding-errors)
        (decode-text-lines (%line-test-octets input bom-p) :keep-cr keep-cr)
      (expect (typep lines 'simple-vector) :to-be t)
      (expect lines :to-equalp (%line-test-lines expected))
      (expect (text-layout-bom-p layout) :to-be bom-p)
      (expect (text-layout-line-ending layout) :to-be ending)
      (expect (text-layout-final-newline-p layout) :to-be final-newline)
      (expect encoding-errors :to-be 0)))

  (it "replaces invalid UTF-8 and counts the replacement"
    (let ((bytes (join-octets (string-bytes "before ")
                              (octets #xFF)
                              (string-bytes (format nil "~%after")))))
      (multiple-value-bind (lines layout encoding-errors)
          (decode-text-lines bytes)
        (expect lines
                :to-equalp
                (vector (concatenate 'string "before " (string (code-char #xFFFD)))
                        "after"))
        (expect (text-layout-line-ending layout) :to-be :lf)
        (expect encoding-errors :to-be 1)))))
(describe "aitools.text.domain line range edges"
  (it "walks, counts, and indexes only the requested byte range"
    (let ((bytes (string-bytes (format nil "zero~%one~%two"))))
      (expect (map-lines (lambda (line start end)
                           (declare (ignore start end))
                           line)
                         bytes :start 5 :end 9)
              :to-be 1)
      (expect (count-lines bytes :start 5 :end 9) :to-be 1)
      (expect (count-lines bytes :start 5 :end 8) :to-be 1)
      (expect (count-lines bytes :start 9 :end 9) :to-be 0)
      (let ((index (build-line-index bytes :start 5 :end 9)))
        (expect (line-index-count index) :to-be 1)
        (expect (multiple-value-list (line-index-bounds index bytes 1))
                :to-equal (list 5 8)))))
  (it "computes line content ends for LF, CRLF, and empty lines"
    (let ((content-end (symbol-function (quote aitools.text.domain::%content-end))))
      (expect (funcall content-end (octets 65 13 10) 0 2) :to-be 1)
      (expect (funcall content-end (octets 65 10) 0 1) :to-be 1)
      (expect (funcall content-end (octets 13 10) 0 1) :to-be 0)
      (expect (funcall content-end (octets 10) 0 0) :to-be 0))))

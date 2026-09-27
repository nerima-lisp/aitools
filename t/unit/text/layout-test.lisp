;;;; t/unit/text/layout-test.lisp
;;;;
;;;; Byte-level text facts: binary sniffing, BOM, line endings, final
;;;; newline, and the line model that keeps a BOM out of line 1.
(in-package #:aitools.text.test)

(defun collect-lines (bytes &rest keys)
  (let ((lines '()))
    (apply #'map-lines (lambda (line start end)
                         (push (list line (map 'string #'code-char (subseq bytes start end))) lines)
                         nil)
           bytes keys)
    (nreverse lines)))

(describe "aitools.text.domain binary sniffing"
  (it "calls a file binary only for a NUL within the first 8 KiB"
    (expect (binary-octets-p (string-bytes "plain text")) :to-be-falsy)
    (expect (binary-octets-p (octets 65 0 66)) :to-be-truthy)
    (let ((late (make-array 9000 :element-type '(unsigned-byte 8) :initial-element 65)))
      (fill late 0 :start 8500 :end 8501)
      (expect (binary-octets-p late) :to-be-falsy)
      (fill late 0 :start 8191 :end 8192)
      (expect (binary-octets-p late) :to-be-truthy))))

(describe "aitools.text.domain layout"
  (it "detects the BOM, line ending, and final newline"
    (let ((layout (detect-text-layout (join-octets (octets #xEF #xBB #xBF) (string-bytes (format nil "a~C~%b~C~%" #\Return #\Return))))))
      (expect (text-layout-bom-p layout) :to-be-truthy)
      (expect (text-layout-line-ending layout) :to-be :crlf)
      (expect (text-layout-final-newline-p layout) :to-be-truthy))
    (let ((layout (detect-text-layout (string-bytes (format nil "a~%b")))))
      (expect (text-layout-bom-p layout) :to-be-falsy)
      (expect (text-layout-line-ending layout) :to-be :lf)
      (expect (text-layout-final-newline-p layout) :to-be-falsy)))

  (it "reports mixed and absent line endings, ignoring a lone CR"
    (expect (line-ending-style (string-bytes (format nil "a~%b~C~%" #\Return))) :to-be :mixed)
    (expect (line-ending-style (string-bytes (format nil "a~Cb" #\Return))) :to-be :none)
    (expect (utf8-bom-length (octets #xEF #xBB)) :to-be 0)))

(describe "aitools.text.domain lines"
  (it "splits on LF, strips CR before LF, and counts an unterminated last line"
    (expect (collect-lines (string-bytes (format nil "one~C~%two~%three" #\Return)))
            :to-equal '((1 "one") (2 "two") (3 "three")))
    (expect (count-lines (string-bytes (format nil "a~%b~%"))) :to-be 2)
    (expect (count-lines (string-bytes (format nil "a~%b"))) :to-be 2)
    (expect (count-lines (octets)) :to-be 0)
    (expect (collect-lines (string-bytes (format nil "~%~%"))) :to-equal '((1 "") (2 ""))))

  (it "keeps the BOM out of line 1 when started after it"
    (let ((bytes (join-octets (octets #xEF #xBB #xBF) (string-bytes (format nil "first~%second")))))
      (expect (collect-lines bytes :start (utf8-bom-length bytes)) :to-equal '((1 "first") (2 "second")))))

  (it "stops when the continuation returns :STOP"
    (let ((seen 0))
      (expect (map-lines (lambda (line start end) (declare (ignore start end)) (setf seen line) (when (= line 2) :stop))
                         (string-bytes (format nil "a~%b~%c~%")))
              :to-be 2)
      (expect seen :to-be 2)))

  (it "runs DO-LINES bodies with a stack-allocated continuation"
    (let ((total 0) (bytes (string-bytes (format nil "ab~%cde~%"))))
      (do-lines ((line start end) bytes)
        (incf total (- end start))
        nil)
      (expect total :to-be 5)))

  (it "indexes lines for random access by number"
    (let* ((bytes (string-bytes (format nil "zero~C~%one~%two" #\Return)))
           (index (build-line-index bytes)))
      (expect (line-index-count index) :to-be 3)
      (multiple-value-bind (start end) (line-index-bounds index bytes 1)
        (expect (list start end) :to-equal '(0 4)))
      (multiple-value-bind (start end) (line-index-bounds index bytes 3)
        (expect (map 'string #'code-char (subseq bytes start end)) :to-equal "two"))
      (expect (line-index-bounds index bytes 4) :to-be-falsy))))

(defun %count-line-bytes (bytes)
  (let ((total 0))
    (declare (type fixnum total))
    (do-lines ((line start end) bytes)
      (incf total (- end start))
      nil)
    total))

(describe "aitools.text.domain line walk allocation"
  (it "allocates less than one byte per line walked"
    ;; GET-BYTES-CONSED advances in allocation-region steps, so the bound is
    ;; per line rather than zero: 100000 lines consing even one cons each
    ;; would exceed it sixteenfold.
    (let ((bytes (make-array 400000 :element-type '(unsigned-byte 8) :initial-element 97)))
      (loop for i from 3 below 400000 by 4 do (setf (aref bytes i) 10))
      (%count-line-bytes bytes)
      (let* ((before (sb-ext:get-bytes-consed))
             (total (%count-line-bytes bytes))
             (consed (- (sb-ext:get-bytes-consed) before)))
        (expect total :to-be 300000)
        (expect consed :to-be-less-than 100000)))))

(describe "aitools.text.domain layout at range edges"
  (it "counts a line feed at START as LF, not CRLF, even after a CR outside the range"
    (expect (line-ending-style (octets 10)) :to-be :lf)
    (expect (line-ending-style (octets 13 10 10) :start 1) :to-be :lf)
    (expect (final-newline-p (octets)) :to-be-falsy)
    (expect (final-newline-p (octets 65 10) :start 2) :to-be-falsy))

  (it "counts an incomplete U+FFFD encoding at the end as an error, a complete one as genuine"
    (expect (multiple-value-list (decode-utf8 (octets #x41 #xEF))) :to-equal (list (format nil "A~C" (code-char #xFFFD)) 1))
    (expect (nth-value 1 (decode-utf8 (octets #xEF #xBF #xBD #xEF #xBF))) :to-be 1)))

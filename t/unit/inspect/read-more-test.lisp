(in-package #:aitools.inspect.test)

(describe "read windows and continuations"
  (it-each (("the byte path" () "aitools read big.txt --range 41:120")
            ("the decoded path" (:escape-invisible t) "aitools read big.txt --range 41:120 --escape-invisible"))
      "names the window before a truncated --tail on ~A"
      (label options next)
    (declare (ignore label))
    (multiple-value-bind (kind fields)
        (apply #'read-file `(("/work/big.txt" . ,(numbered-lines 200))) "big.txt" :tail 100 options)
      (expect kind :to-be :partial)
      (expect (field fields "start_line") :to-be 121)
      (expect (length (field fields "lines")) :to-be 80)
      (expect (field fields "next_commands") :to-equal (list next))))

  (it-each (("the byte path" ())
            ("the decoded path" (:escape-invisible t)))
      "reads an empty file as no lines on ~A"
      (label options)
    (declare (ignore label))
    (multiple-value-bind (kind fields) (apply #'read-file '(("/work/e.txt" . "")) "e.txt" options)
      (expect kind :to-be :ok)
      (expect (field fields "lines") :to-be nil)
      (expect (field fields "total_lines") :to-be 0)))

  (it "reports a --range on an empty file as selection.no-match without candidates"
    (multiple-value-bind (kind fields) (read-file '(("/work/e.txt" . "")) "e.txt" :range "1")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "selection.no-match")
      (expect (getf fields :candidates) :to-be nil)))

  (it "shows no lines for --between --exclusive around adjacent lines"
    (multiple-value-bind (kind fields) (read-file '(("/work/b.txt" . "a
b
c
")) "b.txt" :between '("^a" "^b") :exclusive t)
      (expect kind :to-be :ok)
      (expect (field fields "start_line") :to-be 2)
      (expect (field fields "lines") :to-be nil)))

  (it "names every --match line, keeping --invert, when --max-lines cuts them"
    (multiple-value-bind (kind fields) (read-file `(("/work/m.txt" . ,(numbered-lines 5))) "m.txt"
                                                  :match "line [12]" :invert t :max-lines 1)
      (expect kind :to-be :partial)
      (expect (field fields "line_numbers") :to-equal '(3))
      (expect (field fields "next_commands")
              :to-equal '("aitools read m.txt --match 'line [12]' --invert --max-lines 3")))))

(describe "read hex and strings arguments"
  (it-each (("--bytes without --as hex" (:bytes "0:4") "--bytes needs --as hex")
            ("--encoding with --as hex" (:as "hex" :encoding "shift_jis")
             "--encoding and --escape-invisible apply to --as text only")
            ("--escape-invisible with --as strings" (:as "strings" :escape-invisible t)
             "--encoding and --escape-invisible apply to --as text only")
            ("a reversed --bytes span" (:as "hex" :bytes "5:2") "--bytes \"5:2\" is not S:E or S:")
            ("a --bytes without a colon" (:as "hex" :bytes "12") "--bytes \"12\" is not S:E or S:"))
      "rejects ~A"
      (label options message)
    (declare (ignore label))
    (multiple-value-bind (kind fields) (apply #'read-file '(("/work/h.bin" . "0123456789")) "h.bin" options)
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (getf fields :message) :to-contain message)))

  (it "dumps --bytes S: to the end of the file"
    (multiple-value-bind (kind fields) (read-file '(("/work/h.bin" . "0123456789")) "h.bin" :as "hex" :bytes "6:")
      (expect kind :to-be :ok)
      (expect (list (field fields "start") (field fields "end")) :to-equal '(6 10))
      (expect (json-object-get (first (field fields "rows")) "hex") :to-equal "36 37 38 39")))

  (it "names the next 256 bytes after the default hex window"
    (let ((octets (make-array 300 :element-type '(unsigned-byte 8) :initial-element 65)))
      (multiple-value-bind (kind fields) (read-file `(("/work/h.bin" . ,octets)) "h.bin" :as "hex")
        (expect kind :to-be :ok)
        (expect (field fields "end") :to-be 256)
        (expect (field fields "next_commands") :to-equal '("aitools read h.bin --as hex --bytes 256:300")))))
  (it "marks a requested hex span partial when --max-lines truncates it"
    (let ((octets (make-array 40 :element-type '(unsigned-byte 8) :initial-element 65)))
      (multiple-value-bind (kind fields)
          (read-file `(("/work/h.bin" . ,octets)) "h.bin" :as "hex" :bytes "0:32" :max-lines 1)
        (expect kind :to-be :partial)
        (expect (field fields "end") :to-be 16)
        (expect (field fields "next_commands")
                :to-equal '("aitools read h.bin --as hex --bytes 16:40")))))

  (it "names every run when --max-lines cuts --as strings"
    (let ((octets (concatenate '(vector (unsigned-byte 8)) (string-bytes "first") #(0) (string-bytes "second") #(0))))
      (multiple-value-bind (kind fields) (read-file `(("/work/s.bin" . ,octets)) "s.bin" :as "strings" :max-lines 1)
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 2)
        (expect (field fields "next_commands")
                :to-equal '("aitools read s.bin --as strings --min-length 4 --max-lines 2")))))
  (it "returns no runs from an empty --as strings input"
    (let ((octets (make-array 0 :element-type '(unsigned-byte 8))))
      (multiple-value-bind (kind fields)
          (read-file `(("/work/s.bin" . ,octets)) "s.bin" :as "strings")
        (expect kind :to-be :ok)
        (expect (field fields "strings") :to-be nil)
        (expect (field fields "total") :to-be 0))))
  (it "cuts a long --as strings run and names the remaining bytes"
    (let ((octets (make-array 20000 :element-type '(unsigned-byte 8) :initial-element 65)))
      (multiple-value-bind (kind fields)
          (read-file `(("/work/s.bin" . ,octets)) "s.bin" :as "strings")
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 1)
        (let ((run (first (field fields "strings"))))
          (expect (length (json-object-get run "text")) :to-be 16384)
          (expect (json-object-get run "cut") :to-be t))
        (expect (field fields "next_commands")
                :to-equal '("aitools read s.bin --as hex --bytes 16384:16640"))))))

(describe "missing-path candidates on a large workspace"
  (it "ranks only the first 20000 scanned paths"
    (let ((files (cons '("/work/z/target-near.txt" . "late")
                       (loop for index below 20000
                             collect (cons (format nil "/work/a/f~5,'0D.txt" index) "")))))
      (multiple-value-bind (kind fields) (read-file files "z/target-nea.txt")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.not-found")
        ;; z/target-near.txt is the nearest path but lies past the scan bound.
        (expect (find "z/target-near.txt" (getf fields :candidates)
                      :key (lambda (candidate) (json-object-get candidate "path")) :test #'string=)
                :to-be nil))))

  (it "offers z/target-near.txt when the workspace is small"
    (multiple-value-bind (kind fields) (read-file '(("/work/z/target-near.txt" . "x") ("/work/a/f.txt" . ""))
                                                  "z/target-nea.txt")
      (expect kind :to-be :error)
      (expect (json-object-get (first (getf fields :candidates)) "path") :to-equal "z/target-near.txt"))))

(defun call-with-source-read-spies (function)
  (let ((range-reads '())
        (chunk-reads '())
        (whole-file-reads '())
        (range-reader #'aitools.text.application:source-read-range)
        (chunk-reader #'aitools.text.application:source-call-with-chunks)
        (octets-reader #'aitools.text.application:source-read-octets)
        (sniffed-reader #'aitools.text.application:source-read-sniffed))
    (cl-weave:with-mocked-functions
        (((symbol-function 'aitools.text.application:source-read-range)
          (lambda (source path start end)
            (push (list path start end) range-reads)
            (funcall range-reader source path start end)))
         ((symbol-function 'aitools.text.application:source-call-with-chunks)
          (lambda (source path size function)
            (push (list path size) chunk-reads)
            (funcall chunk-reader source path size function)))
         ((symbol-function 'aitools.text.application:source-read-octets)
          (lambda (source path)
            (push path whole-file-reads)
            (funcall octets-reader source path)))
         ((symbol-function 'aitools.text.application:source-read-sniffed)
          (lambda (source path count continue-p)
            (push path whole-file-reads)
            (funcall sniffed-reader source path count continue-p))))
      (funcall function
               (lambda ()
                 (list :ranges (nreverse range-reads)
                       :chunks (nreverse chunk-reads)
                       :whole-files (nreverse whole-file-reads)))))))

(defun %patterned-octets (size)
  (let ((octets (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (index size octets) (setf (aref octets index) (mod index 251)))))

(describe "read --as hex and --as strings through shared test ports"
  (it "requests a bounded range and avoids whole-file read APIs"
    (let* ((octets (%patterned-octets (* 1024 1024)))
           (ports (make-test-ports :files (list (cons "/work/big.bin" octets)))))
      (call-with-source-read-spies
       (lambda (reads)
         (multiple-value-bind (kind fields)
             (run-flow #'read-flow ports "big.bin" :as "hex" :bytes "600000:600020")
           (expect kind :to-be :ok)
           (expect (list (field fields "size") (field fields "start") (field fields "end"))
                   :to-equal (list (* 1024 1024) 600000 600020))
           (expect (mapcar (lambda (row) (json-object-get row "offset")) (field fields "rows"))
                   :to-equal '(600000 600016))
           (expect (json-object-get (second (field fields "rows")) "hex")
                   :to-equal (format nil "~{~(~2,'0x~)~^ ~}" (loop for index from 600016 below 600020 collect (mod index 251))))
           (let* ((events (funcall reads)) (range (first (getf events :ranges))))
             (expect (length (getf events :ranges)) :to-be 1)
             (destructuring-bind (path start end) range
               (expect path :to-equal "/work/big.bin")
               (expect (<= start 600000) :to-be t)
               (expect (>= end 600020) :to-be t)
               (expect (<= (- end start) (+ 20 (* 2 4096))) :to-be t))
             (expect (getf events :whole-files) :to-be nil)))))))

  (it "lists strings chunk by chunk across run and character boundaries"
    (let* ((octets (make-array (* 3 65536) :element-type '(unsigned-byte 8) :initial-element 0))
           (straddling (string-bytes (format nil "straddle~Crun" (code-char #x3042)))))
      (replace octets (string-bytes "first") :start1 10)
      ;; The run starts before the 64 KiB chunk edge and its 3-byte character
      ;; sits across it.
      (replace octets straddling :start1 (- 65536 9))
      (replace octets (string-bytes "last") :start1 (- (* 3 65536) 4))
      (let ((ports (make-test-ports :files (list (cons "/work/big.bin" octets)))))
        (call-with-source-read-spies
         (lambda (reads)
           (multiple-value-bind (kind fields)
               (run-flow #'read-flow ports "big.bin" :as "strings")
             (expect kind :to-be :ok)
             (expect (field fields "size") :to-be (* 3 65536))
             (expect (mapcar (lambda (item) (list (json-object-get item "offset") (json-object-get item "text")))
                             (field fields "strings"))
                     :to-equal (list (list 10 "first")
                                     (list (- 65536 9) (format nil "straddle~Crun" (code-char #x3042)))
                                     (list (- (* 3 65536) 4) "last")))
             (let* ((events (funcall reads)) (chunk (first (getf events :chunks))))
               (expect (length (getf events :chunks)) :to-be 1)
               (expect chunk :to-equal '("/work/big.bin" 65536))
               (expect (getf events :whole-files) :to-be nil)))))))))

(describe "read cuts a line longer than the per-line byte cap"
  (flet ((long-file (line) `(("/work/long.txt" . ,(format nil "~A~%short~%" line)))))
    (it-each (("the byte path" ())
              ("the decoded path" (:escape-invisible t))
              ("a --match selection" (:match "a")))
        "returns the first 16384 bytes, marks the line, and names the --as hex rest on ~A"
        (label options)
      (declare (ignore label))
      (multiple-value-bind (kind fields)
          (apply #'read-file (long-file (make-string 50000 :initial-element #\a)) "long.txt" options)
        (expect kind :to-be :partial)
        (expect (length (first (field fields "lines"))) :to-be 16384)
        (expect (field fields "cut_lines") :to-equal '(1))
        (expect (json-false-value-p (field fields "truncated")) :to-be nil)
        (expect (field fields "next_commands") :to-contain "aitools read long.txt --as hex --bytes 16384:16640")))

    (it "cuts before a multi-byte character the cap would split"
      (multiple-value-bind (kind fields)
          (read-file (long-file (make-string 6000 :initial-element (code-char #x3042))) "long.txt")
        (expect kind :to-be :partial)
        (expect (field fields "lines") :to-equal (list (make-string 5461 :initial-element (code-char #x3042)) "short"))
        (expect (field fields "encoding_errors") :to-be 0)
        (expect (field fields "next_commands") :to-contain "aitools read long.txt --as hex --bytes 16383:16639")))

    (it "leaves a line of exactly 16384 bytes whole"
      (multiple-value-bind (kind fields) (read-file (long-file (make-string 16384 :initial-element #\a)) "long.txt")
        (expect kind :to-be :ok)
        (expect (length (first (field fields "lines"))) :to-be 16384)
        (expect (field fields "cut_lines") :to-be nil)))
    (it "cuts by characters with --encoding and still marks the line"
      (multiple-value-bind (kind fields)
          (read-file (long-file (make-string 20000 :initial-element #\a)) "long.txt" :encoding "iso-8859-1")
        (expect kind :to-be :partial)
        (expect (length (first (field fields "lines"))) :to-be 16384)
        (expect (field fields "cut_lines") :to-equal '(1))))))

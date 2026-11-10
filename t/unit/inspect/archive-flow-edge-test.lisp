(in-package #:aitools.inspect.test)

(describe "archive read flow edges"
  (it "uses text mode and the default line limit for a single-member archive"
    (let* ((text (format nil "~{line~D~%~}" (loop for line below 81 collect line)))
           (archive
             (aitools.text.domain:write-zip
              (list (aitools.text.domain:make-archive-member
                     :name "lines.txt" :kind :file :mode #o644 :mtime 0
                     :data (%as-octets text))))))
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.inspect.application:archive-read-flow
                    (make-test-boundaries :files `(("/work/lines.zip" . ,archive)))
                    "lines.zip" "lines.txt")
        (expect kind :to-be :partial)
        (expect (field fields "mode") :to-equal "text")
        (expect (field fields "entry") :to-equal "lines.txt")
        (expect (length (field fields "lines")) :to-be 80)
        (expect (field fields "total_lines") :to-be 81))))

  (it "forwards missing archive candidates and repairs"
    (let ((archive
            (aitools.text.domain:write-zip
             (list (aitools.text.domain:make-archive-member
                    :name "inside.txt" :kind :file :mode #o644 :mtime 0
                    :data (%as-octets "content"))))))
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.inspect.application:archive-read-flow
                    (make-test-boundaries :files `(("/work/a.zip" . ,archive)))
                    "a.zpi" "inside.txt")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.not-found")
        (expect (json-object-get (first (getf fields :candidates)) "path")
                :to-equal "a.zip")
        (expect (getf (first (getf fields :repairs)) :command)
                :to-equal "aitools find a.zpi"))))
)
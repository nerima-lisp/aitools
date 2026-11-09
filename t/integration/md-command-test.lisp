(in-package #:cl-user)

(defpackage #:aitools.integration.md-command-test
            (:use #:cl)
            (:shadowing-import-from #:cl-weave #:describe)
            (:import-from #:cl-weave #:it #:expect))

(in-package #:aitools.integration.md-command-test)

(defun %md-ok (root arguments command)
  (multiple-value-bind (code envelope stream) (aitools.test.support:run-aitools root arguments)
    (expect code :to-be 0)
    (expect stream :to-be :stdout)
    (expect envelope :to-match-envelope "command" command "status" "ok")
    envelope))

(defun %md-error (root arguments command error-code)
  (multiple-value-bind (code envelope stream) (aitools.test.support:run-aitools root arguments)
    (expect code :to-be (aitools.protocol.domain:error-code-exit-code error-code))
    (expect stream :to-be :stderr)
    (expect envelope :to-match-envelope "command" command "status" "error")
    (expect (aitools.test.support:envelope-value envelope "error" "code") :to-equal error-code)
    envelope))

(describe "Markdown commands through the loaded dispatcher"
          (it "lists duplicate headings and reads each selected section completely"
              (aitools.test.support:with-workspace (root state :chdir t)
                                                   (declare (ignore state))
                                                   (let ((source
                                                          (format nil
                                                                  "---~%title: demo~%---~%# Same~%first~%## Child~%nested~%# Same~%second~%")))
                                                     (aitools.test.support:write-text
                                                      (concatenate 'string root "/guide.md")
                                                      source)
                                                     (let* ((listed
                                                             (%md-ok root '("md" "headings" "guide.md") "md headings"))
                                                            (entries
                                                             (coerce
                                                              (aitools.test.support:envelope-value listed "headings")
                                                              'list)))
                                                       (expect
                                                        (mapcar
                                                         (lambda (entry)
                                                           (list (aitools.test.support:envelope-value entry "section")
                                                                 (aitools.test.support:envelope-value entry "heading")
                                                                 (aitools.test.support:envelope-value entry "level")
                                                                 (aitools.test.support:envelope-value entry "line")))
                                                         entries)
                                                        :to-equal
                                                        '((1 "Same" 1 4) (2 "Child" 2 6) (3 "Same" 1 8)))
                                                       (let* ((start (search "# Same" source))
                                                              (end (search "# Same" source :start2 (1+ start)))
                                                              (read-first
                                                               (%md-ok root '("md" "read" "guide.md" "1") "md read")))
                                                         (expect
                                                          (aitools.test.support:envelope-value read-first "content")
                                                          :to-equal
                                                          (subseq source start end)))
                                                       (let ((read-last
                                                              (%md-ok root '("md" "read" "guide.md" "3") "md read")))
                                                         (expect
                                                          (aitools.test.support:envelope-value read-last "content")
                                                          :to-equal
                                                          (subseq source
                                                                  (search "# Same"
                                                                          source
                                                                          :start2
                                                                          (1+ (search "# Same" source))))))))
                                                   (%md-error root
                                                              '("md" "read" "guide.md" "99")
                                                              "md read"
                                                              "selection.no-match")
                                                   (%md-error root
                                                              '("md" "read" "guide.md" "zero")
                                                              "md read"
                                                              "argument.invalid"))))

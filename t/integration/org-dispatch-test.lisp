;;;; t/integration/org-dispatch-test.lisp
(in-package #:cl-user)

(defpackage #:aitools.integration.org-dispatch-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect))

(in-package #:aitools.integration.org-dispatch-test)

(defun %expect-ok-envelope (code envelope stream command)
  (expect code :to-be 0)
  (expect stream :to-be :stdout)
  (expect envelope :to-match-envelope "command" (substitute #\. #\Space command) "status" "ok")
  envelope)

(defun %expect-error-envelope (code envelope stream command error-code absent-fields)
  (expect code :to-be 1)
  (expect stream :to-be :stderr)
  (expect envelope :to-match-envelope "command" (substitute #\. #\Space command) "status" "error")
  (expect (aitools.test.support:envelope-value envelope "error" "code") :to-equal error-code)
  (let ((message (aitools.test.support:envelope-value envelope "error" "message")))
    (expect (and (stringp message) (plusp (length message))) :to-be t))
  (expect (aitools.test.support:envelope-value envelope "error" "exit_code") :to-be code)
  (let ((repair (aitools.test.support:envelope-value envelope "error" "repairs" 0)))
    (expect repair :to-be-truthy)
    (when repair
      (dolist (field (list "action" "detail" "command"))
        (expect (let ((value (aitools.test.support:envelope-value repair field)))
                  (and (stringp value) (plusp (length value)))) :to-be t))
      (expect (aitools.test.support:envelope-value repair "command")
              :to-equal (format nil "aitools schema ~A" command))))
  (dolist (field absent-fields)
    (expect (nth-value 1 (gethash field envelope)) :to-be nil))
  envelope)

(describe "Org read commands through the loaded dispatcher"
  (it "returns outline, query, and export results without changing source bytes"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((path (concatenate 'string root "/siblings.org"))
             (relative "siblings.org")
             (source (format nil "~{~A~%~}"
                             '("#+TODO: TODO | DONE" "* Parent" "** First"
                               "** TODO Second :work:" "* TODO Other :work:"))))
        (aitools.test.support:write-text path source)
        (let ((before (aitools.test.support:read-bytes path)))
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:run-aitools root (list "org" "outline" relative))
            (let* ((result (%expect-ok-envelope code envelope stream "org outline"))
                   (entries (coerce (aitools.test.support:envelope-value result "entries") 'list)))
              (expect (aitools.test.support:envelope-value result "total") :to-be 4)
              (expect (mapcar (lambda (entry)
                                (aitools.test.support:envelope-value entry "name"))
                              entries)
                      :to-equal '("Parent" "First" "Second" "Other"))
              (expect (aitools.test.support:envelope-value result "truncated")
                      :to-be json-kit:+json-false+)))
          (expect (aitools.test.support:read-bytes path) :to-equalp before)
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:run-aitools
               root (list "org" "query" relative "--match" "work/TODO"
                          "--at" "2026-10-19T09:00:00" "--tz" "UTC"))
            (let* ((result (%expect-ok-envelope code envelope stream "org query"))
                   (matches (coerce (aitools.test.support:envelope-value result "matches") 'list)))
              (expect (aitools.test.support:envelope-value result "count") :to-be 2)
              (expect (mapcar (lambda (match)
                                  (list (aitools.test.support:envelope-value match "line")
                                        (aitools.test.support:envelope-value match "level")
                                        (aitools.test.support:envelope-value match "title")))
                                matches)
                      :to-equal '((4 2 "Second") (5 1 "Other")))
              (expect (mapcar (lambda (match)
                                (mapcar (lambda (part) (aitools.test.support:envelope-value part "index"))
                                        (coerce (aitools.test.support:envelope-value match "heading_path") 'list)))
                              matches)
                      :to-equal '((1 1) (1)))))
          (expect (aitools.test.support:read-bytes path) :to-equalp before)
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:run-aitools
               root (list "org" "query" relative "--match" "LEVEL=1"
                          "--at" "2026-10-19T09:00:00" "--tz" "UTC"))
            (let* ((result (%expect-ok-envelope code envelope stream "org query"))
                   (matches (coerce (aitools.test.support:envelope-value result "matches") 'list))
                   (parent (first matches)))
              (expect (aitools.test.support:envelope-value result "count") :to-be 2)
              (expect (aitools.test.support:envelope-value parent "title") :to-equal "Parent")
              (multiple-value-bind (todo present) (gethash "todo" parent)
                (expect present :to-be-truthy)
                (expect todo :to-be json-kit:+json-null+))
              (multiple-value-bind (tags present) (gethash "tags" parent)
                (expect present :to-be-truthy)
                (expect (coerce tags (quote list)) :to-equal nil))))
          (expect (aitools.test.support:read-bytes path) :to-equalp before)
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:run-aitools
               root (list "org" "export" relative "--backend" "html"))
            (let* ((result (%expect-ok-envelope code envelope stream "org export"))
                   (content (aitools.test.support:envelope-value result "content")))
              (expect (aitools.test.support:envelope-value result "backend") :to-equal "html")
              (expect (and (stringp content) (search "Parent" content) (search "Other" content)) :to-be-truthy)))
          (expect (aitools.test.support:read-bytes path) :to-equalp before)))))

  (it "rejects NUL, UTF-32 BOM, and binary inputs without changing source bytes"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let ((late-nul (make-array 8193
                                 :element-type (quote (unsigned-byte 8))
                                 :initial-element 65)))
        (setf (aref late-nul 8192) 0)
        (dolist (case (list (list "late-nul.org" late-nul)
                            (list "utf32.org" #(255 254 0 0 42 0 0 0))
                            (list "binary.org" #(71 73 70 56 0 1))))
          (let* ((relative (first case))
                 (path (concatenate (quote string) root "/" relative)))
            (aitools.test.support:write-bytes path (second case))
            (let ((before (aitools.test.support:read-bytes path)))
              (multiple-value-bind (code envelope stream)
                  (aitools.test.support:run-aitools root (list "org" "outline" relative))
                (%expect-error-envelope
                 code envelope stream "org outline" "input.unsupported-format"
                 (list "entries" "total" "truncated")))
              (expect (aitools.test.support:read-bytes path) :to-equalp before)))))))
  (it "checks outline, query, and export contracts through the loaded dispatcher"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let ((outline-path (concatenate (quote string) root "/outline.org")))
        (aitools.test.support:write-text
         outline-path
         (format nil "#+TODO: TODO | DONE~%* Parent~%** First~%** TODO Second~%* TODO Other~%"))
        (let ((before (aitools.test.support:read-bytes outline-path)))
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:run-aitools root
                                                (list "org" "outline" "outline.org" "--limit" "2"))
            (let* ((result (%expect-ok-envelope code envelope stream "org outline"))
                   (entries (coerce (aitools.test.support:envelope-value result "entries")
                                    (quote list))))
              (expect (aitools.test.support:envelope-value result "total") :to-be 4)
              (expect (aitools.test.support:envelope-value result "truncated")
                      :to-be t)
              (expect (mapcar (lambda (entry)
                                (aitools.test.support:envelope-value entry "name"))
                              entries)
                      :to-equal (list "Parent" "First"))))
          (expect (aitools.test.support:read-bytes outline-path) :to-equalp before)
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:run-aitools root (list "org" "outline" "missing.org"))
            (%expect-error-envelope code envelope stream "org outline" "input.not-found"
                                    (list "entries" "total" "truncated")))
          (let ((invalid-path (concatenate (quote string) root "/invalid.org")))
            (aitools.test.support:write-bytes invalid-path #(255))
            (let ((invalid-before (aitools.test.support:read-bytes invalid-path)))
              (multiple-value-bind (code envelope stream)
                  (aitools.test.support:run-aitools root (list "org" "outline" "invalid.org"))
                (%expect-error-envelope code envelope stream "org outline" "input.not-utf8"
                                        (list "entries" "total" "truncated")))
              (expect (aitools.test.support:read-bytes invalid-path)
                      :to-equalp invalid-before)))))
      (let ((query-path (concatenate (quote string) root "/query.org")))
        (aitools.test.support:write-text
         query-path (format nil "#+TODO: TODO | DONE~%* TODO Scheduled~%SCHEDULED: <2026-03-08 Sun 02:00>~%"))
        (let ((before (aitools.test.support:read-bytes query-path)))
          (let* ((syntax-envelope
                   (multiple-value-bind (code envelope stream)
                       (aitools.test.support:run-aitools
                        root (list "org" "query" "query.org" "--match" "work/(TODO)"
                                   "--at" "2026-10-19T09:00:00" "--tz" "UTC"))
                     (%expect-error-envelope code envelope stream "org query"
                                             "input.syntax-error" (list "matches" "count"))))
                 (diagnostic
                   (aitools.test.support:envelope-value syntax-envelope "error" "diagnostics" 0)))
            (expect (plusp (length (aitools.test.support:envelope-value
                                   syntax-envelope "error" "diagnostics")))
                    :to-be-truthy)
            (expect (aitools.test.support:envelope-value diagnostic "line") :to-be 1)
            (expect (and (integerp (aitools.test.support:envelope-value diagnostic "col"))
                         (plusp (aitools.test.support:envelope-value diagnostic "col")))
                    :to-be-truthy)
            (expect (and (stringp (aitools.test.support:envelope-value diagnostic "message"))
                         (plusp (length
                                 (aitools.test.support:envelope-value diagnostic "message"))))
                    :to-be-truthy))
          (expect (aitools.test.support:read-bytes query-path) :to-equalp before)
          (labels ((query-arguments (at zone policy)
                     (append (list "org" "query" "query.org" "--match"
                                   "SCHEDULED<=\"<now>\"" "--at" at)
                             (when zone (list "--tz" zone))
                             (when policy (list "--dst-policy" policy))))
                   (expect-query-error (arguments)
                     (multiple-value-bind (code envelope stream)
                         (aitools.test.support:run-aitools root arguments)
                       (%expect-error-envelope code envelope stream "org query"
                                               "argument.invalid" (list "matches" "count")))
                     (expect (aitools.test.support:read-bytes query-path) :to-equalp before))
                   (match-count (policy)
                     (multiple-value-bind (code envelope stream)
                         (aitools.test.support:run-aitools
                          root (query-arguments "2026-03-08T02:30:00"
                                                "America/New_York" policy))
                       (let ((result (%expect-ok-envelope code envelope stream "org query")))
                         (prog1 (aitools.test.support:envelope-value result "count")
                           (expect (aitools.test.support:read-bytes query-path)
                                   :to-equalp before))))))
            (expect-query-error
             (query-arguments "2026-10-19T09:00:30" "UTC" nil))
            (expect-query-error
             (query-arguments "2026-03-08T02:30:00" nil nil))
            (expect-query-error
             (query-arguments "2026-03-08T02:30:00" "America/New_York" "strict"))
            (expect (match-count "earlier") :to-be 0)
            (expect (match-count "compatible") :to-be 1))))
      (let ((mailto-path (concatenate (quote string) root "/mailto.org")))
        (aitools.test.support:write-text
         mailto-path (format nil "* Contact~%[[mailto:ops@example.test][mail]]~%"))
        (let ((before (aitools.test.support:read-bytes mailto-path)))
          (dolist (backend (list "html" "gfm" "ascii"))
            (multiple-value-bind (code envelope stream)
                (aitools.test.support:run-aitools
                 root (list "org" "export" "mailto.org" "--backend" backend))
              (let* ((result (%expect-ok-envelope code envelope stream "org export"))
                     (content (aitools.test.support:envelope-value result "content")))
                (expect (aitools.test.support:envelope-value result "backend") :to-equal backend)
                (expect (stringp content) :to-be-truthy)
                (cond ((string= backend "html")
                       (expect (search "<a href=\"mailto:ops@example.test\">mail</a>" content)
                               :to-be-truthy))
                      ((string= backend "gfm")
                       (expect (search "[mail](<mailto:ops@example.test>)" content)
                               :to-be-truthy))
                      (t
                       (expect (and (search "[mail]" content)
                                    (search "[mail] <mailto:ops@example.test>" content))
                               :to-be-truthy)))))
            (expect (aitools.test.support:read-bytes mailto-path) :to-equalp before))))
      (dolist (fixture
               (list (list "file.org" (format nil "* Link~%[[file:target.org][file]]~%"))
                     (list "internal.org"
                           (format nil "* Link~%[[#target][internal]]~%* Target~%"))))
        (let ((path (concatenate (quote string) root "/" (first fixture))))
          (aitools.test.support:write-text path (second fixture))
          (let ((before (aitools.test.support:read-bytes path)))
            (dolist (backend (list "html" "gfm" "ascii"))
              (multiple-value-bind (code envelope stream)
                  (aitools.test.support:run-aitools
                   root (list "org" "export" (first fixture) "--backend" backend))
                (%expect-error-envelope code envelope stream "org export"
                                        "input.unsupported-format" (list "content")))
              (expect (aitools.test.support:read-bytes path) :to-equalp before))))))))

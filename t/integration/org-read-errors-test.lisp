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
    (aitools.test.support:with-workspace (root state :chdir t)
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
            (expect (file-length stream) :to-be (1+ limit)))))))
  (it "maps an Org document parse error to a source diagnostic without outline results"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((path (concatenate (quote string) root "/bad-todo.org"))
             (source "#+TODO:")
             (expected "TODO sequence has no keywords"))
        (aitools.test.support:write-text path source)
        (let ((before (aitools.test.support:read-bytes path)))
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:run-aitools root (list "org" "outline" "bad-todo.org"))
            (%expect-error-envelope code envelope stream "org outline" "input.syntax-error"
                                    (list "entries" "total" "truncated"))
            (expect (aitools.test.support:envelope-value envelope "error" "message")
                    :to-equal expected)
            (let ((diagnostics
                    (aitools.test.support:envelope-value envelope "error" "diagnostics")))
              (expect (length diagnostics) :to-be 1)
              (let ((diagnostic
                      (aitools.test.support:envelope-value
                       envelope "error" "diagnostics" 0)))
                (expect (aitools.test.support:envelope-value diagnostic "line") :to-be 1)
                (expect (aitools.test.support:envelope-value diagnostic "col") :to-be 1)
                (expect (aitools.test.support:envelope-value diagnostic "message")
                        :to-equal expected)))
            (expect (aitools.test.support:read-bytes path) :to-equalp before))))))
  (it "maps an Org parser resource limit to refusal.too-large without outline results"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((relative "deep.org")
             (path (concatenate (quote string) root "/deep.org"))
             (source (format nil "~A Deep~%" (make-string 257 :initial-element #\*)))
             (bytes (sb-ext:string-to-octets source :external-format :utf-8))
             (input-limit
               (org-kit:limit-value (org-kit:make-limits) :max-input-length)))
        (expect (< (length bytes) input-limit) :to-be t)
        (aitools.test.support:write-bytes path bytes)
        (multiple-value-bind (code envelope stream)
            (aitools.test.support:run-aitools root (list "org" "outline" relative))
          (%expect-error-envelope code envelope stream "org outline" "refusal.too-large"
                                  (list "entries" "total" "truncated"))
          (expect (search "Resource limit exceeded"
                          (aitools.test.support:envelope-value envelope "error" "message"))
                  :to-be-truthy)
          (expect (aitools.test.support:read-bytes path) :to-equalp bytes)))))
  (it "rejects an empty Org path and a non-directory workspace root"
    (aitools.test.support:with-workspace (root state)
      (declare (ignore state))
      (multiple-value-bind (code envelope stream)
          (aitools.test.support:run-aitools root (list "org" "outline" ""))
        (%expect-error-envelope code envelope stream "org outline" "argument.invalid"
                                (list "entries" "total" "truncated"))
        (expect (aitools.test.support:envelope-value envelope "error" "message")
                :to-equal "an Org file path is required"))
      (let ((file-root (concatenate (quote string) root "/root-file")))
        (aitools.test.support:write-text file-root "not a workspace")
        (multiple-value-bind (code envelope stream)
            (aitools.test.support:run-aitools file-root
                                              (list "org" "outline" "unused.org"))
          (%expect-error-envelope code envelope stream "org outline" "input.not-found"
                                  (list "entries" "total" "truncated"))
          (expect (aitools.test.support:envelope-value envelope "error" "message")
                  :to-equal (format nil "workspace root ~A is not a directory"
                                    file-root)))))))
(describe "Org read input validation through the loaded dispatcher"
  (it "validates query inputs and export backend without success fields"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((relative "read.org")
             (path (concatenate 'string root "/read.org")))
        (aitools.test.support:write-text path (format nil "* Task~%"))
        (let ((before (aitools.test.support:read-bytes path)))
          (labels ((expect-error (arguments command absent-fields)
                     (multiple-value-bind (code envelope stream)
                         (aitools.test.support:run-aitools root arguments)
                       (%expect-error-envelope code envelope stream command
                                               "argument.invalid" absent-fields))))
            (expect-error (list "org" "query" relative
                                "--at" "2026-10-19T09:00:00" "--tz" "UTC")
                          "org query" '("matches" "count"))
            (expect-error (list "org" "query" relative "--match" "TODO"
                                "--at" "2026-02-30T09:00:00" "--tz" "UTC")
                          "org query" '("matches" "count"))
            (expect-error (list "org" "query" relative "--match" "TODO"
                                "--at" "2026-10-19T09:00:00+00:00" "--tz" "UTC")
                          "org query" '("matches" "count"))
            (expect-error (list "org" "query" relative "--match" "TODO"
                                "--at" "2026-10-19T09:00:00" "--tz" "UTC"
                                "--dst-policy" "unknown")
                          "org query" '("matches" "count"))
            (expect-error (list "org" "query" relative "--match" "")
                          "org query" '("matches" "count"))
            (expect-error (list "org" "query" relative
                                "--match" "TODO" "--at" "" "--tz" "UTC")
                          "org query" '("matches" "count"))
            (expect-error (list "org" "query" relative
                                "--match" "TODO" "--at" "2026-10-19T09:00:00" "--tz" "")
                          "org query" '("matches" "count"))
            (expect-error (list "org" "outline" relative "--limit" "-1")
                          "org outline" '("entries" "total" "truncated"))
            (expect-error (list "org" "export" relative)
                          "org export" '("backend" "content"))
            (expect-error (list "org" "export" relative "--backend" "pdf")
                          "org export" '("backend" "content")))
          (expect (aitools.test.support:read-bytes path) :to-equalp before))))))

(cl-weave:describe-skip-if (zerop (sb-posix:geteuid))
    "Org read permission errors through the loaded dispatcher (skipped as root)"
  (it "returns environment.io when an Org source file cannot be read"
    (aitools.test.support:with-workspace (root state)
      (declare (ignore state))
      (let* ((path (concatenate 'string root "/unreadable.org"))
             (native (uiop:native-namestring path)))
        (aitools.test.support:write-text path "* Private~%")
        (let ((before (aitools.test.support:read-bytes path)))
          (sb-posix:chmod native 0)
          (unwind-protect
               (multiple-value-bind (code envelope stream)
                   (aitools.test.support:run-aitools
                    root (list "org" "outline" path))
                 (%expect-error-envelope code envelope stream "org outline"
                                         "environment.io"
                                         '("entries" "total" "truncated"))
                 (expect (aitools.test.support:envelope-value
                          envelope "error" "message")
                         :to-equal (format nil "file ~A cannot be read" path)))
            (sb-posix:chmod native #o600))
          (expect (aitools.test.support:read-bytes path) :to-equalp before))))))


(in-package #:aitools.integration.org-dispatch-test)

(describe "Org read input and error contracts through the loaded dispatcher"
  (it "reports the invalid UTF-8 byte offset and omits outline results"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((path (concatenate 'string root "/invalid.org"))
             (bytes #(65 42 255 66)))
        (aitools.test.support:write-bytes path bytes)
        (multiple-value-bind (code envelope stream)
            (aitools.test.support:run-aitools root (list "org" "outline" "invalid.org"))
          (%expect-error-envelope code envelope stream "org outline" "input.not-utf8"
                                  '("entries" "total" "truncated"))
          (let ((diagnostic
                  (aitools.test.support:envelope-value envelope "error" "diagnostics" 0)))
            (expect (aitools.test.support:envelope-value diagnostic "offset") :to-be 2)
            (expect (aitools.test.support:envelope-value diagnostic "message")
                    :to-equal "Org input is not valid UTF-8"))
          (expect (aitools.test.support:read-bytes path) :to-equalp bytes)))))
  (it "rejects complete and partial UTF-16/32 signatures through the binary sniff"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (dolist (fixture
               (list (list "utf16le.org" #(#xFF #xFE #x2A #x00 #x20 #x00 #x54 #x00 #x0A #x00)
                           "UTF-16 little-endian")
                     (list "utf16be.org" #(#xFE #xFF #x00 #x2A #x00 #x20 #x00 #x54 #x00 #x0A)
                           "UTF-16 big-endian")
                     (list "utf32le.org" #(#xFF #xFE #x00 #x00 #x2A #x00 #x00 #x00)
                           "UTF-32 little-endian")
                     (list "utf32be.org" #(#x00 #x00 #xFE #xFF #x00 #x00 #x00 #x2A)
                           "UTF-32 big-endian")
                     (list "utf16le-short.org" #(#xFF #xFE #x00)
                           "UTF-16 little-endian")
                     (list "utf16be-short.org" #(#xFE #xFF #x00)
                           "UTF-16 big-endian")
                     (list "short-binary.org" #(0) "binary file")))
        (destructuring-bind (relative bytes encoding) fixture
          (let ((path (concatenate 'string root "/" relative)))
            (aitools.test.support:write-bytes path bytes)
            (multiple-value-bind (code envelope stream)
                (aitools.test.support:run-aitools root (list "org" "outline" relative))
              (%expect-error-envelope code envelope stream "org outline"
                                      "input.unsupported-format"
                                      '("entries" "total" "truncated"))
              (expect (search encoding
                              (aitools.test.support:envelope-value envelope "error" "message"))
                      :to-be-truthy)
              (expect (aitools.test.support:read-bytes path) :to-equalp bytes)))))))
  (it "maps missing source files to query and export error envelopes"
    (aitools.test.support:with-workspace (root state)
      (declare (ignore state))
      (multiple-value-bind (code envelope stream)
          (aitools.test.support:run-aitools
           root (list "org" "query" "missing.org" "--match" "TODO"
                      "--at" "2026-10-19T09:00:00" "--tz" "UTC"))
        (%expect-error-envelope code envelope stream "org query" "input.not-found"
                                '("matches" "count")))
      (multiple-value-bind (code envelope stream)
          (aitools.test.support:run-aitools
           root (list "org" "export" "missing.org" "--backend" "html"))
        (%expect-error-envelope code envelope stream "org export" "input.not-found"
                                '("backend" "content")))))
  (it "rejects an unknown query time zone without query result fields"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let ((path (concatenate 'string root "/query.org")))
        (aitools.test.support:write-text path (format nil "#+TODO: TODO | DONE~%* TODO Task~%"))
        (multiple-value-bind (code envelope stream)
            (aitools.test.support:run-aitools
             root (list "org" "query" "query.org" "--match" "TODO"
                        "--at" "2026-10-19T09:00:00" "--tz" "No/Such_Zone"))
          (%expect-error-envelope code envelope stream "org query" "argument.invalid"
                                  '("matches" "count"))))))
  (it "reports query syntax diagnostics when cyclic TAGS have no parser offset"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((path (concatenate 'string root "/cyclic-tags.org"))
             (source (format nil "#+TAGS: { First : Second } { Second : First }~%* Entry :x:~%")))
        (aitools.test.support:write-text path source)
        (let ((before (aitools.test.support:read-bytes path)))
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:run-aitools
               root (list "org" "query" "cyclic-tags.org" "--match" "First"
                          "--at" "2026-10-19T09:00:00" "--tz" "UTC"))
            (%expect-error-envelope code envelope stream "org query" "input.syntax-error"
                                    '("matches" "count"))
            (let* ((error (aitools.test.support:envelope-value envelope "error"))
                   (diagnostic (aitools.test.support:envelope-value error "diagnostics" 0))
                   (message (aitools.test.support:envelope-value diagnostic "message")))
              (expect (aitools.test.support:envelope-value diagnostic "line") :to-be 1)
              (expect (aitools.test.support:envelope-value diagnostic "col") :to-be 1)
              (expect (and (stringp message) (plusp (length message))) :to-be t)
              (expect (nth-value 1 (gethash "offset" diagnostic)) :to-be nil)))
          (expect (aitools.test.support:read-bytes path) :to-equalp before)))))
  (it "rejects a query with --at omitted before opening the source"
    (aitools.test.support:with-workspace (root state)
      (declare (ignore state))
      (multiple-value-bind (code envelope stream)
          (aitools.test.support:run-aitools
           root (list "org" "query" "missing.org" "--match" "TODO" "--tz" "UTC"))
        (%expect-error-envelope code envelope stream "org query" "argument.invalid"
                                '("matches" "count"))
        (expect (aitools.test.support:envelope-value envelope "error" "message")
                :to-equal "--match, --at, and --tz are required")
        (expect (nth-value 1
                  (gethash "diagnostics"
                           (aitools.test.support:envelope-value envelope "error")))
                :to-be nil))))
  (it "returns JSON null levels for unnamed outline blocks"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((path (concatenate (quote string) root "/unnamed.org"))
             (source (format nil
                             "#+BEGIN_SRC lisp~%(+ 1 2)~%#+END_SRC~%#+BEGIN_EXAMPLE~%plain text~%#+END_EXAMPLE~%")))
        (aitools.test.support:write-text path source)
        (let ((before (aitools.test.support:read-bytes path)))
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:run-aitools root (list "org" "outline" "unnamed.org"))
            (let* ((result (%expect-ok-envelope code envelope stream "org outline"))
                   (entries (aitools.test.support:envelope-value result "entries"))
                   (source-block (elt entries 0))
                   (example-block (elt entries 1)))
              (expect (length entries) :to-be 2)
              (expect (aitools.test.support:envelope-value source-block "kind")
                      :to-equal "src-block")
              (expect (aitools.test.support:envelope-value source-block "name")
                      :to-equal "lisp")
              (expect (aitools.test.support:envelope-value source-block "level")
                      :to-be json-kit:+json-null+)
              (expect (aitools.test.support:envelope-value source-block "line") :to-be 1)
              (expect (aitools.test.support:envelope-value example-block "kind")
                      :to-equal "example-block")
              (expect (aitools.test.support:envelope-value example-block "level")
                      :to-be json-kit:+json-null+)
              (expect (aitools.test.support:envelope-value example-block "line") :to-be 4)))
          (expect (aitools.test.support:read-bytes path) :to-equalp before))))))

(describe "Org export contracts through the loaded dispatcher"
  (it "rejects malformed source export modes through all backends"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (dolist (fixture
               (list (list "unknown"
                           (format nil "#+BEGIN_SRC text :exports html~%code~%#+END_SRC~%")
                           "Unsupported :exports mode")
                     (list "duplicate"
                           (format nil "#+BEGIN_SRC text :exports code :exports none~%code~%#+END_SRC~%")
                           "Duplicate :exports source block argument")
                     (list "value-less"
                           (format nil "#+BEGIN_SRC text :exports~%code~%#+END_SRC~%")
                           "Missing :exports source block value")))
        (let* ((relative (format nil "~A.org" (first fixture)))
               (path (format nil "~A/~A" root relative))
               (source (second fixture))
               (expected-message (third fixture)))
          (aitools.test.support:write-text path source)
          (dolist (backend (list "html" "gfm" "ascii"))
            (multiple-value-bind (code envelope stream)
                (aitools.test.support:run-aitools
                 root (list "org" "export" relative "--backend" backend))
              (%expect-error-envelope code envelope stream "org export"
                                      "input.unsupported-format"
                                      (list "backend" "content"))
              (let ((diagnostic
                      (aitools.test.support:envelope-value envelope "error" "diagnostics" 0)))
                (expect (aitools.test.support:envelope-value diagnostic "line") :to-be 1)
                (expect (aitools.test.support:envelope-value diagnostic "col") :to-be 1)
                (expect (aitools.test.support:envelope-value diagnostic "message")
                        :to-equal expected-message))))
          (expect (aitools.test.support:read-bytes path) :to-equalp
                  (sb-ext:string-to-octets source :external-format :utf-8))))))

  (it "returns an export error envelope for unsupported Diary syntax"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((path (concatenate 'string root "/diary.org"))
             (source (format nil "* Diary~%%%(diary-float t 4 2)~%")))
        (aitools.test.support:write-text path source)
        (dolist (backend '("html" "gfm" "ascii"))
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:run-aitools
               root (list "org" "export" "diary.org" "--backend" backend))
            (%expect-error-envelope code envelope stream "org export"
                                    "input.unsupported-format" '("backend" "content"))
            (expect (aitools.test.support:envelope-value envelope "error" "diagnostics" 0)
                    :to-be-truthy)))
        (expect (aitools.test.support:read-bytes path) :to-equalp
                (sb-ext:string-to-octets source :external-format :utf-8)))))
  (it "rejects unknown link schemes with the actual Org link type"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((path (concatenate 'string root "/unsafe.org"))
             (source (format nil "* Link~%[[javascript:alert(1)][unsafe]]~%")))
        (aitools.test.support:write-text path source)
        (dolist (backend '("html" "gfm" "ascii"))
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:run-aitools
               root (list "org" "export" "unsafe.org" "--backend" backend))
            (%expect-error-envelope code envelope stream "org export"
                                    "input.unsupported-format" '("backend" "content"))
            (let ((diagnostic
                    (aitools.test.support:envelope-value envelope "error" "diagnostics" 0)))
              (expect (aitools.test.support:envelope-value diagnostic "line") :to-be 2)
              (expect (aitools.test.support:envelope-value diagnostic "col") :to-be 1)
              (expect (aitools.test.support:envelope-value diagnostic "message")
                      :to-equal "export does not support Org link type fuzzy"))))
        (expect (aitools.test.support:read-bytes path) :to-equalp
                (sb-ext:string-to-octets source :external-format :utf-8))))))

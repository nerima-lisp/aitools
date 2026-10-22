(in-package #:aitools.e2e.test)

(defparameter +org-export-invalid-source-mode-fixtures+
  (list
   (list "unknown"
         (format nil "#+BEGIN_SRC text :exports html~%code~%#+END_SRC~%")
         "Unsupported :exports mode")
   (list "duplicate"
         (format nil "#+BEGIN_SRC text :exports code :exports none~%code~%#+END_SRC~%")
         "Duplicate :exports source block argument")
   (list "value-less"
         (format nil "#+BEGIN_SRC text :exports~%code~%#+END_SRC~%")
         "Missing :exports source block value")))

(describe "aitools e2e org export"
  (define-org-read-case
    ("org export rejects malformed source :exports arguments without content" ws)
    (dolist (fixture +org-export-invalid-source-mode-fixtures+)
      (let* ((relative (format nil "~A.org" (first fixture)))
             (source (second fixture))
             (expected-message (third fixture)))
        (put ws relative source)
        (let ((before (file-octets ws relative)))
          (dolist (backend (list "html" "gfm" "ascii"))
            (let* ((envelope (org-error ws (list "org" "export" relative
                                                  "--backend" backend)
                                        "org.export" "input.unsupported-format"))
                   (present (nth-value 1 (gethash "content" envelope)))
                   (diagnostics (jlist (jget envelope "error" "diagnostics")))
                   (diagnostic (first diagnostics)))
              (expect present :to-be nil)
              (expect (nth-value 1 (gethash "backend" envelope)) :to-be nil)
              (expect (plusp (length diagnostics)) :to-be t)
              (expect (jget diagnostic "line") :to-be 1)
              (expect (jget diagnostic "col") :to-be 1)
              (expect (jget diagnostic "message") :to-equal expected-message)))
          (expect (file-octets ws relative) :to-equalp before)))))

  (define-org-read-case ("org export preserves ordered lists in every backend" ws)
    (put ws "ordered.org" (format nil "1. ordered~%"))
    (let ((before (file-octets ws "ordered.org")))
      (dolist (backend (list "html" "gfm" "ascii"))
        (let* ((envelope (org-ok ws (list "org" "export" "ordered.org"
                                          "--backend" backend)
                                 "org.export"))
               (content (jget envelope "content")))
          (expect (jget envelope "backend") :to-equal backend)
          (expect (stringp content) :to-be t)
          (if (string= backend "html")
              (expect (search "<ol" content) :to-be-truthy)
              (expect (search "1. ordered" content) :to-be-truthy))))
      (expect (file-octets ws "ordered.org") :to-equalp before)))
  (define-org-read-case
   ("org export preserves mailto links in every backend" ws)
   (put ws "mailto.org"
        (format nil "* Contact~%[[mailto:ops@example.test][mail]]~%"))
   (let ((before (file-octets ws "mailto.org")))
     (dolist (backend (list "html" "gfm" "ascii"))
       (let* ((envelope (org-ok ws (list "org" "export" "mailto.org"
                                         "--backend" backend)
                                "org.export"))
              (content (jget envelope "content")))
         (expect (jget envelope "backend") :to-equal backend)
         (expect (stringp content) :to-be t)
         (cond
          ((string= backend "html")
           (expect-org-fragments
            content (list "<a href=\"mailto:ops@example.test\">mail</a>")))
          ((string= backend "gfm")
           (expect-org-fragments
            content (list "[mail](<mailto:ops@example.test>)")))
          (t
           (expect-org-fragments
            content (list "[mail]" "[mail] <mailto:ops@example.test>"))))))
     (expect (file-octets ws "mailto.org") :to-equalp before)))
  (define-org-read-case
   ("org export rejects unknown link schemes without partial content" ws)
   (put ws "unsafe.org"
        (format nil "* Link~%[[javascript:alert(1)][unsafe]]~%"))
   (let ((before (file-octets ws "unsafe.org")))
     (dolist (backend (list "html" "gfm" "ascii"))
       (let* ((envelope (org-error ws (list "org" "export" "unsafe.org"
                                            "--backend" backend)
                                   "org.export" "input.unsupported-format"))
              (present (nth-value 1 (gethash "content" envelope)))
              (diagnostics (jlist (jget envelope "error" "diagnostics")))
              (diagnostic (first diagnostics)))
         (expect present :to-be nil)
         (expect (jget diagnostic "line") :to-be 2)
         (expect (jget diagnostic "col") :to-be 1)
         (expect (jget diagnostic "message")
                 :to-equal "export does not support Org link type fuzzy")))
     (expect (file-octets ws "unsafe.org") :to-equalp before)))
  (define-org-read-case ("org export includes metadata in the documented backends" ws)
    (put ws "metadata.org"
         (format nil "#+TITLE: ~A~%#+AUTHOR: ~A~%#+DATE: ~A~%#+LANGUAGE: en~%* Content~%"
                "Quarterly Report" "Ada Lovelace" "2026-10-19"))
    (let ((before (file-octets ws "metadata.org")))
      (dolist (backend '("html" "gfm" "ascii"))
        (let* ((envelope (org-ok ws
                                 (list "org" "export" "metadata.org" "--backend" backend)
                                 "org.export"))
               (content (jget envelope "content")))
          (expect (jget envelope "backend") :to-equal backend)
          (expect (stringp content) :to-be t)
          (cond ((string= backend "html")
                 (expect-org-fragments
                  content '("Quarterly Report" "Ada Lovelace" "2026-10-19" "lang=\"en\"")))
                ((string= backend "gfm")
                 (dolist (metadata '("Quarterly Report" "Ada Lovelace" "2026-10-19"))
                   (expect (search metadata content) :to-be nil)))
                (t
                 (expect-org-fragments
                  content '("QUARTERLY REPORT" "Ada Lovelace" "2026-10-19"))))))
      (expect (file-octets ws "metadata.org") :to-equalp before))))
(describe "aitools e2e org export modes"
  (define-org-read-case ("org export applies all source :exports modes across backends" ws)
    (dolist (fixture
             (list (list "omitted" nil t nil)
                   (list "code" "code" t nil)
                   (list "results" "results" nil t)
                   (list "both" "both" t t)
                   (list "none" "none" nil nil)))
      (destructuring-bind (label mode expected-code-p expected-results-p) fixture
        (let* ((relative (format nil "~A.org" label))
               (source
                 (format nil
                         (concatenate 'string
                                      "#+BEGIN_SRC text~A~%AITOOLS-ORG-EXPORT-CODE-MARKER~%"
                                      "#+END_SRC~%~%#+RESULTS:~%: AITOOLS-ORG-EXPORT-RESULT-MARKER~%")
                         (if mode (format nil " :exports ~A" mode) ""))))
          (put ws relative source)
          (let ((before (file-octets ws relative)))
            (dolist (backend (list "html" "gfm" "ascii"))
              (let* ((envelope (org-ok ws (list "org" "export" relative
                                                "--backend" backend)
                                       "org.export"))
                     (content (jget envelope "content")))
                (expect (jget envelope "backend") :to-equal backend)
                (expect (stringp content) :to-be t)
                (expect (list label backend
                              (not (null (search "AITOOLS-ORG-EXPORT-CODE-MARKER" content))))
                        :to-equal (list label backend expected-code-p))
                (expect (list label backend
                              (not (null (search "AITOOLS-ORG-EXPORT-RESULT-MARKER" content))))
                        :to-equal (list label backend expected-results-p))))
            (expect (file-octets ws relative) :to-equalp before)))))))


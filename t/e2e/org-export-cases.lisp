;;;; t/e2e/org-export-cases.lisp
(in-package #:aitools.e2e.test)

(defparameter +org-export-source-mode-fixture+
  (format nil
          (concatenate 'string
                       "#+BEGIN_SRC text~%source-default-marker~%#+END_SRC~%"
                       "#+BEGIN_SRC text :exports code~%source-code-marker~%#+END_SRC~%"
                       "#+BEGIN_SRC text :exports both~%source-both-no-results-marker~%#+END_SRC~%"
                       "#+BEGIN_SRC text :exports both~%source-both-results-marker~%#+END_SRC~%"
                       "#+RESULTS:~%: source-both-attached-results-marker~%")))


(defparameter +org-export-invalid-source-mode-fixtures+
  `(("unknown" . ,(format nil "#+BEGIN_SRC text :exports html~%code~%#+END_SRC~%"))
    ("duplicate" . ,(format nil "#+BEGIN_SRC text :exports code :exports none~%code~%#+END_SRC~%"))
    ("value-less" . ,(format nil "#+BEGIN_SRC text :exports~%code~%#+END_SRC~%"))
    ("equals" . ,(format nil "#+BEGIN_SRC text :exports=both~%code~%#+END_SRC~%"))))

(describe "aitools e2e org export"
  (define-org-read-case ("org export preserves supported source block modes in every backend" ws)
    (put ws "source.org" +org-export-source-mode-fixture+)
    (let ((before (file-octets ws "source.org")))
      (dolist (backend (list "html" "gfm" "ascii"))
        (let* ((envelope (org-ok ws (list "org" "export" "source.org"
                                          "--backend" backend)
                                 "org.export"))
               (content (jget envelope "content")))
          (expect (jget envelope "backend") :to-equal backend)
          (expect (stringp content) :to-be t)
          (dolist (marker '("source-default-marker"
                          "source-code-marker"
                          "source-both-no-results-marker"
                          "source-both-results-marker"
                          "source-both-attached-results-marker"))
            (expect (search marker content) :to-be-truthy))))
      (expect (file-octets ws "source.org") :to-equalp before)))
  (define-org-read-case ("org export refuses source modes the pinned renderer cannot honor" ws)
    (dolist (fixture
             `(("code-results" .
                ,(format nil "#+BEGIN_SRC text :exports code~%code-marker~%#+END_SRC~%#+RESULTS:~%: result-marker~%"))
               ("results" .
                ,(format nil "#+BEGIN_SRC text :exports results~%code-marker~%#+END_SRC~%"))
               ("none" .
                ,(format nil "#+BEGIN_SRC text :exports none~%code-marker~%#+END_SRC~%"))
               ("default-results" .
                ,(format nil "#+BEGIN_SRC text~%code-marker~%#+END_SRC~%#+RESULTS:~%: result-marker~%"))))
      (let ((path (format nil "source-~A.org" (car fixture))))
        (put ws path (cdr fixture))
        (let ((before (file-octets ws path)))
          (dolist (backend (list "html" "gfm" "ascii"))
            (let* ((envelope (org-error ws
                                        (list "org" "export" path "--backend" backend)
                                        "org.export" "input.unsupported-format"))
                   (present (nth-value 1 (gethash "content" envelope))))
              (expect present :to-be nil)))
          (expect (file-octets ws path) :to-equalp before)))))
  (define-org-read-case ("org export rejects invalid source block exports modes without content" ws)
    (dolist (fixture +org-export-invalid-source-mode-fixtures+)
      (put ws "invalid-exports.org" (cdr fixture))
      (let ((before (file-octets ws "invalid-exports.org")))
        (dolist (backend (list "html" "gfm" "ascii"))
          (let* ((envelope (org-error ws
                                      (list "org" "export" "invalid-exports.org"
                                            "--backend" backend)
                                      "org.export" "input.unsupported-format"))
                 (present (nth-value 1 (gethash "content" envelope))))
            (expect present :to-be nil)))
        (expect (file-octets ws "invalid-exports.org") :to-equalp before))))
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
         (expect (plusp (jget diagnostic "col")) :to-be t)))
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


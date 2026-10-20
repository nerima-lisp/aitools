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

(describe "aitools e2e org source block export"
  (define-org-read-case ("org export preserves supported source block modes in every backend" ws)
    (put ws "source.org" +org-export-source-mode-fixture+)
    (let ((before (file-octets ws "source.org")))
      (dolist (backend (list "html" "gfm" "ascii"))
        (let* ((envelope (org-ok ws (list "org" "export" "source.org"
                                          "--backend" backend)
                                 "org export"))
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
                                        "org export" "input.unsupported-format"))
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
                                      "org export" "input.unsupported-format"))
                 (present (nth-value 1 (gethash "content" envelope))))
            (expect present :to-be nil)))
        (expect (file-octets ws "invalid-exports.org") :to-equalp before)))))

